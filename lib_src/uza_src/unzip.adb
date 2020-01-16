with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Interfaces;                        use Interfaces;
with UnZ_Glob;                          use UnZ_Glob;
with UnZ_IO;                            use UnZ_IO;
with UnZ_CRC;                           use UnZ_CRC;
with UnZ_Tabl;                          use UnZ_Tabl;
with UnZ_Head;                          use UnZ_Head;
with Unchecked_Deallocation;

-- Copy, unshrink, expand, explode, inflate:

with UnZ_Copy, UnZ_Unsh, UnZ_Expa, UnZ_Expl, UnZ_Infl;

package body Unzip is

  use Byte_IO;

--------------------------------------------------
-- *The* internal 1-file unzipping procedure.   --
-- Input must be _open_ and won't be _closed_ ! --
--------------------------------------------------

procedure Unzipfile ( out_name        : String;
                      read_name       : boolean; -- name is be read, not given
                      header_offset   : in out file_size_type;
                      feedback        : feedback_proc;
                      file_exists     : resolve_conflict_proc;
                      tell_data       : tell_data_proc;
                      test_only       : boolean:= false ) is

  offset: Byte_IO.count:= Byte_IO.count(header_offset);

  local_header        : t_localheader;
  local_header_buffer : IO_buf( 1..30 );

  originalcrc: unsigned_32;
  
  data_descriptor: boolean;
  method: pkzip_method;

  function Exist(name:String) return Boolean is
    f: File_Type;
    begin
      Open(f,in_file,name);
      Close(f);
      return True;
    exception
      when Name_Error => return False;
    end;

  skip_this_file: boolean:= false;

  procedure Create_outfile( possible_name: String ) is
    new_name : String( 1..400 );
    new_name_length : Natural;
    begin
      if file_exists /= null and then Exist( possible_name ) then
        loop
          case current_user_attitude is
            when yes | no | rename => -- then ask for this name too
              file_exists( possible_name, current_user_attitude,
                           new_name, new_name_length );
            when yes_to_all | none | abort_now =>
              exit; -- nothing to decide: previous decision was definitive
          end case;
          exit when not (
            current_user_attitude = rename and then   -- new name exists too!
            Exist( new_name( 1..new_name_length ) ) );
        end loop;

        -- User has decided.
        case current_user_attitude is
          when yes | yes_to_all =>
            skip_this_file:= false;
            Create( outfile, out_file, possible_name );
          when no | none =>
            skip_this_file:= true;
          when rename =>
            skip_this_file:= false;
            Create( outfile, out_file, new_name( 1..new_name_length ) );
          when abort_now =>
            raise User_abort;
        end case;
          
      else -- no name conflict or non-interactive (file_exists=null)

        skip_this_file:= false;
        Create( outfile, out_file, possible_name );
      end if;
    end Create_outfile;

BEGIN
  method:= unknown;
  Unz_IO.current_feedback:= feedback; -- we connect the feedback procedure

  if test_only then
    write_mode:= just_test;
  else
    write_mode:= write_to_file;
  end if;

  begin
    Set_index ( infile, offset );
  exception
    when others => raise Read_Error;
  end;

  begin
    for i in local_header_buffer'range loop
      Read( infile, local_header_buffer(i) );
    end loop;
  exception
    when others => raise Read_Error;
  end;

  Interpret( local_header_buffer, local_header );

  case  local_header.zip_type  is
    when stored   => method:= store;
    when shrunk   => method:= shrink;
    when reduced_1=> method:= reduce_1;
    when reduced_2=> method:= reduce_2;
    when reduced_3=> method:= reduce_3;
    when reduced_4=> method:= reduce_4;
    when imploded => method:= implode;
    when deflated => method:= deflate;
    when others   => null;
  end case;
  -- calculate offset of data

  offset := offset + Count(
            local_header.filename_len    +
            local_header.extra_field_len +
            local_header_buffer'length   );

  data_descriptor:= (local_header.bit_flag AND 8) /= 0;

  IF not data_descriptor THEN          -- Sizes and crc are at the beginning
    compsize    := local_header.compress_size;
    uncompsize  := local_header.uncompress_size;
    originalcrc := local_header.crc_32;
  ELSE
    compsize    := file_size_type'last;  -- Don't get a sudden Zip_EOF!
    uncompsize  := file_size_type'last;
    originalcrc := 0;
  END IF;

  IF  (unsigned_32(2 ** integer(local_header.zip_type)) AND SupportedMethods)
       = 0 THEN
    raise Not_Supported;
  END IF;

  IF (local_header.bit_flag AND 1) /= 0 THEN raise Encrypted; end if;

  if read_name then
    declare
      the_name: String(1..Natural(local_header.filename_len));
      b: byte;
    begin
      for i in the_name'range loop
        Read( infile, b );
        the_name(i):= character'val( natural(b) );
      end loop;
      if Tell_data /= null then  -- inform user
        Tell_data( the_name,
                   long_integer(local_header.compress_size),
                   long_integer(local_header.uncompress_size),
                   method );
      end if;
      if the_name = "" or else the_name( the_name'last ) = '/' then
         skip_this_file:= true;
         -- This is a directory name (12-feb-2000)
      elsif write_mode = write_to_file then
        Create_outfile( the_name );
      end if;
    end;
  else  
    if Tell_data /= null then  -- inform user
      Tell_data( out_name,
                 long_integer(local_header.compress_size),
                 long_integer(local_header.uncompress_size),
                 method );
    end if;
    if write_mode = write_to_file then
      Create_outfile( out_name );
    end if;
  end if;

  IF skip_this_file then
    write_mode := just_test;
  end if;

  IF skip_this_file and not data_descriptor then -- we can skip actually
    null;
  else
    -- in case of data descriptor, we must read even to skip!

    -- OS dep.: create directories not yet in path or directories alone !
  
    reachedsize := 0;
    effective_writes := 0;
  
    begin
      Set_index ( infile, offset );
    exception
      when others => raise Read_Error;
    end;
  
    Zip_EOF := False;
  
    InitCrc32 ( crc32val );
  
    -- Unzip correct type
    begin
  
      case  local_header.zip_type  is
        when stored   => UnZ_Copy.copystored;
        when shrunk   => UnZ_Unsh.Unshrink;
        when reduced_1=> UnZ_Expa.Unreduce(1);
        when reduced_2=> UnZ_Expa.Unreduce(2);
        when reduced_3=> UnZ_Expa.Unreduce(3);
        when reduced_4=> UnZ_Expa.Unreduce(4);
        when imploded =>
         UnZ_Expl.Explode( literal_tree => (local_header.bit_flag AND 4) /= 0,
                           slide_8_KB   => (local_header.bit_flag AND 2) /= 0 );
        when deflated => UnZ_Infl.Inflate;
        when others   => raise Not_Supported;
      end case;
  
    exception
      when others =>
        if write_mode = write_to_file then Delete( outfile ); end if;
        raise;
    end;
  
    IF data_descriptor THEN   -- CRC at the end
      Dump_bits ( k  mod 8 );
      originalcrc := Get_via_need_dump;
      compsize    := Get_via_need_dump;
      uncompsize  := Get_via_need_dump;
    END IF;
  
    crc32val := FinalCrc32 ( crc32val );
  
    IF  originalcrc /= crc32val THEN
      if write_mode = write_to_file then Delete( outfile ); end if;
      flush( w );
      if write_mode = write_to_file then Close( outfile ); end if;
      raise CRC_Error;
    ELSE
      null; -- set original date with timedate
    END IF;
 
    if write_mode = write_to_file then Close( outfile ); end if;

  end if; -- not ( skip_this_file and not data_descriptor )

  -- Set the offset on the next zipped file
  header_offset:= header_offset +
    file_size_type(
            local_header.filename_len    +
            local_header.extra_field_len +
            local_header_buffer'length   ) +
    compsize;

  if data_descriptor then 
    header_offset:= header_offset + 3*4;
  end if;

  Unz_IO.current_feedback:= null; -- we disconnect the feedback procedure

exception

  when others=> 
    if write_mode = write_to_file and then Is_open( outfile ) then
      Close( outfile );
    end if;
    Unz_IO.current_feedback:= null; -- we disconnect the feedback procedure
    raise;

end unzipfile;

  the_end: t_end; -- PKZIP end-of-central-directory

  procedure Load_end is
    end_buffer: IO_buf( 1..22 );
    begin
      -- Yes, we must _search_ for it...
      -- because PKWARE put a variable comment _after_ it 8-(

      for i in reverse 1 .. Size(infile) - 21 loop
        Set_index(infile, i);
        for j in end_buffer'range loop
          Read(infile, end_buffer(j));
        end loop;
        begin
          Interpret( end_buffer, the_end );
          return; -- the_end found and filled -> exit
        exception
          when bad_end => null; -- we will try 1 index before...
        end;
      end loop;
    end Load_end;

  -- Internal: find offset of a zipped file by reading sequentially the
  -- central directory :-(

  function Find_offset(name: String) return file_size_type is
    header_buffer: IO_buf( 1..46 );
    header       : t_header;

    begin
      Load_end;
      Set_index(infile, 1 + count(the_end.central_dir_offset));
      for i in 1..the_end.total_entries loop
        for j in header_buffer'range loop
          Read(infile, header_buffer(j));
        end loop;
        Interpret( header_buffer, header );
        declare
          this_name: String(1..natural(header.filename_len));
          b: byte;
        begin
          for k in this_name'range loop
            Read( infile, b );
            this_name(k):= character'val( natural(b) );
          end loop;
          Set_Index( infile, Index( infile ) +
            Count( header.extra_field_len + header.comment_len ) );
          -- Now the whole i_th central directory entry is behind
          if To_Upper(this_name) = To_Upper(name) then
            -- Name found in central directory !
            return 1 + header.local_header_offset;
          end if;
        end;
      end loop;
      raise File_name_not_found;
    end Find_offset;

  -- Internal: find offset of a zipped file using the zip_info tree 8-)

  forgot_to_load_zip_info: exception;

  function Find_offset(info: zip_info; name: String) return long_integer is
    aux: p_dir_node:= info.dir_binary_tree;
    up_name: string:= To_Upper(name);
    begin
      if not info.loaded then raise forgot_to_load_zip_info; end if;
      while aux /= null loop
        if up_name > aux.name then
          aux:= aux.right;
        elsif up_name < aux.name then
          aux:= aux.left;
        else
          return aux.offset; -- file found !
        end if;
      end loop;
      raise File_name_not_found;
    end Find_offset;

  procedure Load_zip_info( from : String; info : out zip_info ) is

    procedure Insert( name: string; offset: long_integer;
                      node: in out p_dir_node ) is
      begin
        if node = null then
          node:= New dir_node'
            ( (name_len => name'length,
               left => null, right => null,
               name => name, offset => offset) );
        elsif name > node.name then
          Insert( name, offset, node.right );
        elsif name < node.name then
          Insert( name, offset, node.left );
        else
          raise Duplicate_name;
        end if;
      end Insert;

    header_buffer: IO_buf( 1..46 );
    header       : t_header;
    p            : p_dir_node:= null;

    begin
      Open_infile( from );
      Load_end;
      Set_index(infile, 1 + count(the_end.central_dir_offset));
      for i in 1..the_end.total_entries loop
        for j in header_buffer'range loop
          Read(infile, header_buffer(j));
        end loop;
        Interpret( header_buffer, header );
        declare
          this_name: String(1..natural(header.filename_len));
          b: byte;
        begin
          for k in this_name'range loop
            Read( infile, b );
            this_name(k):= character'val( natural(b) );
          end loop;
          Set_Index( infile, Index( infile ) +
            Count( header.extra_field_len + header.comment_len ) );
          -- Now the whole i_th central directory entry is behind
          Insert( name   => To_Upper(this_name), 
                  offset => long_integer(1 + header.local_header_offset),
                  node   => p );
        end;
      end loop;
      Close(infile);
      info:= ( loaded          => True,
               zip_file_name   => New String'(from),
               dir_binary_tree => p
             );
    end Load_zip_info;

  procedure Delete_zip_info( info : in out zip_info ) is
    procedure Dispose is new Unchecked_Deallocation( string, p_string );
  
    procedure Delete( p: in out p_dir_node ) is
      procedure Dispose is new Unchecked_Deallocation( dir_node, p_dir_node );
      begin
        if p/=null then
           Delete(p.left);
           Delete(p.right);
           Dispose(p);
           p:= null;
        end if;
      end Delete;

    begin
      if not info.loaded then raise forgot_to_load_zip_info; end if;
      Delete( info.dir_binary_tree );
      Dispose( info.zip_file_name );
    end Delete_zip_info;

  ----------------------------------
  -- Simple extraction procedures --
  ----------------------------------

  procedure Extract( from, what : String;
                     test_only  : boolean:= false) is
    begin
      Extract( from, what, null, null, null, test_only );
    end Extract;

  procedure Extract( from, what, rename : String;
                     test_only  : boolean:= false ) is
    begin
      Extract( from, what, rename, null, null, test_only );
    end Extract;

  procedure Extract_all_files(
                     from : String;
                     test_only  : boolean:= false ) is
    begin
      Extract_all_files( from, null, null, null, test_only );
    end Extract_all_files;

  procedure Extract( from : zip_info; what : String;
                     test_only  : boolean:= false ) is
    begin
      Extract( from, what, null, null, null, test_only );
    end Extract;
    
  procedure Extract( from : zip_info; what, rename : String;
                     test_only  : boolean:= false ) is
    begin
      Extract( from, what, rename, null, null, test_only );
    end Extract;

  -- All previous ones call the following ones, with bogus UI arguments

  ------------------------------------------------------------
  -- All previous extraction procedures, for user interface --
  ------------------------------------------------------------

  procedure Extract( from, what  : String;
                     feedback    : feedback_proc;
                     file_exists : resolve_conflict_proc;
                     tell_data   : tell_data_proc;
                     test_only   : boolean:= false ) is

    header_offset: file_size_type;
    begin
      if feedback = null then
        current_user_attitude:= yes_to_all; -- non-interactive
      end if;
      Open_infile( from );
      header_offset:= Find_offset(what);
      Unzipfile( what, false, header_offset,
                 feedback, file_exists, tell_data, test_only );
      Close(infile);
    end Extract;

  procedure Extract( from, what, rename : String;
                     feedback    : feedback_proc;
                     tell_data   : tell_data_proc;
                     test_only   : boolean:= false ) is

    header_offset: file_size_type;
    begin
      if feedback = null then
        current_user_attitude:= yes_to_all; -- non-interactive
      end if;
      Open_infile( from );
      header_offset:= Find_offset(what);
      Unzipfile( rename, false, header_offset,
                 feedback, null, tell_data, test_only );
      Close(infile);
    end Extract;

  procedure Extract_all_files(
                     from        : String;
                     feedback    : feedback_proc;
                     file_exists : resolve_conflict_proc;
                     tell_data   : tell_data_proc;
                     test_only   : boolean:= false ) is

    header_offset: file_size_type:= 1;
    begin
      if feedback = null then
        current_user_attitude:= yes_to_all; -- non-interactive
      end if;
      Open_infile( from );
      loop -- We simply unzip everything sequentially, until the end !
        Unzipfile( "", true, header_offset,
                   feedback, file_exists, tell_data, test_only );
      end loop;
    exception
      when bad_local_header => Close(infile); -- normal case
      when Zip_file_open_error => raise;      -- couldn't open zip file
      when others => Close(infile); raise;    -- something else wrong
    end Extract_all_files;

  procedure Extract( from : zip_info; what : String;
                     feedback    : feedback_proc;
                     file_exists : resolve_conflict_proc;
                     tell_data   : tell_data_proc;
                     test_only   : boolean:= false ) is

    header_offset: file_size_type;
    begin
      if feedback = null then
        current_user_attitude:= yes_to_all; -- non-interactive
      end if;
      header_offset:= file_size_type( Find_offset(from, what) );
      -- search offset "offline" !
      Open_infile( from.zip_file_name.all );
      Unzipfile( what, false, header_offset,
                 feedback, file_exists, tell_data, test_only );
      Close(infile);
    end Extract;

  procedure Extract( from : zip_info; what, rename : String;
                     feedback    : feedback_proc;
                     tell_data   : tell_data_proc;
                     test_only   : boolean:= false  ) is

    header_offset: file_size_type;
    begin
      if feedback = null then
        current_user_attitude:= yes_to_all; -- non-interactive
      end if;
      header_offset:= file_size_type( Find_offset(from, what) );
      -- search offset "offline" !
      Open_infile( from.zip_file_name.all );
      Unzipfile( rename, false, header_offset,
                 feedback, null, tell_data, test_only );
      Close(infile);
    end Extract;

end Unzip;
