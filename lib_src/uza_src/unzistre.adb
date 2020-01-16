with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Interfaces;                        use Interfaces;
with UnZ_Glob;                          use UnZ_Glob;
with UnZ_IO;                            use UnZ_IO;
with UnZ_CRC;                           use UnZ_CRC;
with UnZ_Tabl;                          use UnZ_Tabl;
with UnZ_Head;                          use UnZ_Head;
with Ada.Unchecked_Deallocation;
with Ada.Streams;                       use Ada.Streams;

-- Copy, unshrink, expand, explode, inflate:

with UnZ_Copy, UnZ_Unsh, UnZ_Expa, UnZ_Expl, UnZ_Infl;


package body Unzip.Streams is

   procedure Dispose is new
     Ada.Unchecked_Deallocation( String, p_String );

   procedure Dispose is new
     Ada.Unchecked_Deallocation( Stream_Element_Array,
                                 p_Stream_Element_Array );

   procedure Dispose is new
     Ada.Unchecked_Deallocation( Unzip_Stream_Type,
                                 Zipped_File_Type );

-- An ugly adaptation of the (equally ugly) code in unzip.adb!

--------------------------------------------------
-- *The* internal 1-file unzipping procedure.   --
-- Input must be _open_ and won't be _closed_ ! --
--------------------------------------------------

procedure Unzipfile ( out_name        : String;
                      header_offset   : in out file_size_type;
                      mem_ptr         : out p_Stream_Element_Array ) is

  use Byte_IO;

  offset: Byte_IO.count:= Byte_IO.count(header_offset);

  local_header        : t_localheader;
  local_header_buffer : IO_buf( 1..30 );

  originalcrc: unsigned_32;

  data_descriptor: boolean;
  method: pkzip_method;

BEGIN
  mem_ptr:= null;
  method:= unknown;
  Unz_IO.current_feedback:= null; -- we connect the feedback procedure
  write_mode := write_to_memory;

  begin
    Byte_IO.Set_index ( infile, offset );
  exception
    when others => raise Read_Error;
  end;

  begin
    for i in local_header_buffer'range loop
      Byte_IO.Read( infile, local_header_buffer(i) );
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

  offset := offset + Byte_IO.Count(
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
    uncompsize  := file_size_type'last; -- would crash mem_ptr allocation!
    originalcrc := 0;
  END IF;

  IF  (unsigned_32(2 ** integer(local_header.zip_type)) AND SupportedMethods)
       = 0 THEN
    raise Not_Supported;
  END IF;

  IF (local_header.bit_flag AND 1) /= 0 THEN raise Encrypted; end if;

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

  mem_ptr:= New Stream_Element_Array( 1 .. Stream_Element_Offset(uncompsize) );

  -- Connect the pointers in Unz_Glob (not task safe!)

  uncompressed_memory:= UnZ_Glob.p_Stream_Element_Array(mem_ptr);
  uncompressed_index := uncompressed_memory'first;

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

  IF data_descriptor THEN   -- CRC at the end
    Dump_bits ( k  mod 8 );
    originalcrc := Get_via_need_dump;
    compsize    := Get_via_need_dump;
    uncompsize  := Get_via_need_dump;
  END IF;

  crc32val := FinalCrc32 ( crc32val );

  IF  originalcrc /= crc32val THEN
    raise CRC_Error;
  END IF;


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

end unzipfile;

  procedure S_Extract( from     : zip_info;
                       what     : String;
                       mem_ptr  : out p_Stream_Element_Array  ) is

    header_offset: file_size_type;
    begin
      header_offset:= file_size_type( Find_offset(from, what) );
      -- search offset "offline" !
      Open_infile( from.zip_file_name.all );
      Unzipfile( what, header_offset, mem_ptr );
      Byte_IO.Close(infile);
    end S_Extract;

-------------------- for exportation:

   procedure Close (File : in out Zipped_File_Type) is
   begin
     if File = null or else File.state = uninitialized then
        raise Use_error;
     end if;
     if File.delete_info_on_closing then
       Delete_zip_info( File.archive_info );
     end if;
     Dispose(File.file_name);
     Dispose(File.uncompressed);
     Dispose(File);
     File:= null;
   end Close;

   function Is_Open (File : in Zipped_File_Type) return Boolean is
   begin
     return File /= null and then File.state /= uninitialized;
   end Is_Open;

   function End_Of_File (File : in Zipped_File_Type) return Boolean is
   begin
     if File = null or else File.state = uninitialized then
        raise Use_error;
     end if;
     return File.state = end_of_zip;
   end End_Of_File;

   procedure Open
     (File         : in out Zipped_File_Type; -- File-in-archive handle
      Archive_Info : in zip_info;             -- loaded by Load_zip_info
      Name         : in String                -- Name of zipped entry
     ) is
   begin
     if File = null then
       File:= New Unzip_Stream_Type;
     elsif File.state /= uninitialized then -- forgot to close last time!
       raise Use_error;
     end if;
     File.archive_info:= Archive_Info;
     File.file_name:= New String' (Name);
     S_Extract( from    =>   File.archive_info,
                what    =>   name,
                mem_ptr =>   File.uncompressed );

     File.index:= File.uncompressed'first;
     File.state:= data_uncompressed;
   end Open;

   procedure Open
     (File         : in out Zipped_File_Type; -- File-in-archive handle
      Archive_Name : in String;               -- Name of archive file
      Name         : in String                -- Name of zipped entry
     ) is

   ti: zip_info;
   begin
     Load_zip_info( Archive_name, ti );
     Open( File, ti, Name );
     File.delete_info_on_closing:= True; -- Close will delete dir tree
   end Open;

   procedure Read
     (Stream : in out Unzip_Stream_Type;
      Item   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset) is

   begin
     if Stream.state = uninitialized then
       raise Use_error;
     end if;
     for i in Item'range loop
       if Stream.state = end_of_zip then
         raise End_error;
       end if;
       Item(i):= Stream.uncompressed(Stream.index);
       Last:= i;
       Stream.index:= Stream.index + 1;
       if Stream.index > Stream.uncompressed'last then
         Stream.state:= end_of_zip;
       end if;
     end loop;
   end Read;


   function Stream (File : Zipped_File_Type) return Stream_Access is
     begin
       return Stream_Access(File);
     end Stream;

   procedure Write
     (Stream : in out Unzip_Stream_Type;
      Item   : in     Stream_Element_Array) is

     write_not_supported: exception;
     begin
       raise write_not_supported;
     end Write;

end Unzip.Streams;
