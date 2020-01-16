with Text_IO;
with UnZ_Glob;                          use UnZ_Glob;
with UnZ_CRC;                           use UnZ_CRC;
with Ada.Streams;                       use Ada.Streams;

package body UnZ_IO is

  procedure Open_infile( in_name: String ) is
    begin
      Byte_IO.Open( infile, Byte_IO.in_file, in_name );
    exception
      when others => raise Zip_file_open_error;
    end Open_infile;

-- ************************** fill inbuf from infile *********************

procedure Read_buffer is
 use Byte_IO;
 BEGIN
  if trace then Text_IO.Put("[Read_buffer..."); end if;

  IF reachedsize > compsize + 2 THEN -- +2: last code is smaller than requested!}
    readpos := inbuf'length; -- Simulates reading -> no blocking
    Zip_EOF := TRUE;
  ELSE
    readpos:= 0;

    begin
      for i in inbuf'range loop           -- (TP's blockread)
        Read( infile, inbuf(i) );
        readpos:= readpos + 1;
      end loop;
    exception
      when End_Error => null;
      -- nothing, we just reached EOF
      -- MUCH faster method than checking End_of_file !

      when others => -- I/O error
        readpos := inbuf'length; -- Simulates reading -> CRC error
        Zip_EOF := TRUE;
    end;
    if readpos = 0 then
      readpos := inbuf'length; -- Simulates reading -> CRC error
      Zip_EOF := TRUE;
    end if;

    reachedsize:= reachedsize + file_size_type(readpos);
    readpos:= readpos - 1;  -- Reason: index of inbuf starts at 0
  END IF;
  inpos:= 0;
  if trace then Text_IO.Put_Line("finished]"); end if;
 END Read_buffer;

-- **** read byte, only used by explode ****

procedure Read_byte ( bt : out byte ) is
 BEGIN
  IF inpos > readpos THEN Read_buffer; END IF;
  bt := inbuf ( inpos );
  inpos := inpos + 1;
 END Read_byte;

-- *********** read at least n bits into the global variable b *************

procedure Need_bits ( n : natural ) is
 BEGIN
  WHILE k < n LOOP
    IF inpos > readpos THEN Read_buffer; END IF;
    b:= b OR shift_left(unsigned_32( inbuf ( inpos ) ), k );
    inpos := inpos + 1;
    k:= k + 8;
  END LOOP;
 END Need_bits;

-- ***************** dump n bits no longer needed from global variable b ****

procedure Dump_bits ( n : natural ) is
 BEGIN
  b := shift_right(b, n );
  k := k - n;
 END Dump_bits;

-- ********************* Flush w bytes directly from slide to file *********

procedure Flush ( w: natural ) is
  use Unzip;
  user_aborting: boolean;
  BEGIN
    if trace then Text_IO.Put("[Flush..."); end if;

    case write_mode is
      when write_to_file =>
        for i in 0..w-1 loop
          Byte_IO.Write(outfile, slide(i));
        end loop;
      when write_to_memory =>
        for i in 0..w-1 loop
          uncompressed_memory(uncompressed_index):=
            Stream_element(slide(i));
          uncompressed_index :=  uncompressed_index + 1;
         end loop;
      when just_test =>
        null;
    end case;

    UpdateCRC( slide( 0..w-1 ) );

    if current_feedback /= null then -- inform user
      effective_writes:= effective_writes + file_size_type(w);
      current_feedback.all(
        percents_done => Natural(
          (100.0 * float(effective_writes)) / float(uncompsize) ),
        user_abort    => user_aborting );
      if user_aborting then
        raise user_abort;
      end if;
    end if;

    if trace then Text_IO.Put_Line("finished]"); end if;
  EXCEPTION
    when others=> raise Write_Error;
  END Flush;

  -- This is to read data in the "Data descriptor" after zipped data:

  function Get_via_need_dump return unsigned_32 is
    result: unsigned_32;
    begin
      Need_bits ( 16 );
      result := b AND 16#FFFF#;
      Dump_bits ( 16 );
      Need_bits ( 16 );
      result := result + shift_left( b  AND  16#FFFF# , 16);
      Dump_bits ( 16 );
      return result;
    end Get_via_need_dump;

end UnZ_IO;
