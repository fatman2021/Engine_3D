with Interfaces;                        use Interfaces;
with UnZ_Glob;                          use UnZ_Glob;
with UnZ_Tabl;                          use UnZ_Tabl;
with UnZ_IO;                            use UnZ_IO;
with UnZ_CRC;                           use UnZ_CRC;
with Unzip;
with Ada.Streams;                       use Ada.Streams;

package body UnZ_Unsh is

-- Note from Pascal version:
-- *************************** unshrink **********************************
-- Written and NOT copyrighted by Christian Ghisler.
-- I have rewritten unshrink because the original
-- function was copyrighted by Mr. Smith of Info-zip
-- This funtion here is now completely FREE!!!!
-- The only right I claim on this code is that
-- noone else claims a copyright on it!

  max_code  : CONSTANT := 8192;
  max_stack : CONSTANT := 8192;
  initial_code_size : CONSTANT := 9;
  final_code_size   : CONSTANT := 13;

  -- Rest of slide=write buffer =766 bytes

  write_max : CONSTANT := wsize-3*(max_code-256)-max_stack-2;

  previous_code: ARRAY (Integer_32'(257)..max_code) OF Integer_32;
  actual_code  : ARRAY (Integer_32'(257)..max_code ) OF Byte;

  next_free : integer_32;      -- Next free code in trie
  write_ptr : integer;         -- Pointer to output buffer

  stack    : IO_Buf ( 0..max_stack );  -- Stack for output
  writebuf : IO_Buf ( 0..write_max );  -- Write buffer

procedure unshrink_flush is
  use Unzip;
  user_aborting: boolean;
  BEGIN
    case write_mode is
      when write_to_file =>
        for i in 0..write_ptr-1 loop
          Byte_IO.Write(outfile, writebuf(i));
        end loop;
      when write_to_memory =>
        for i in 0..write_ptr-1 loop
          uncompressed_memory(uncompressed_index):=
            Stream_element(writebuf(i));
          uncompressed_index :=  uncompressed_index + 1;
         end loop;
      when just_test =>
        null;
    end case;

    UpdateCRC ( writebuf(0 .. write_ptr-1) );

    if current_feedback /= null then -- inform user
      effective_writes:= effective_writes + file_size_type(write_ptr);
      current_feedback.all(
        percents_done => Natural(
          (100.0 * float(effective_writes)) / float(uncompsize) ),
        user_abort    => user_aborting );
      if user_aborting then
        raise user_abort;
      end if;
    end if;
  END;

procedure write_byte( b: byte ) is
  BEGIN
    writebuf ( write_ptr ) := b;
    write_ptr:= write_ptr + 1;
    IF write_ptr > write_max THEN
      unshrink_flush;
      write_ptr := 0;
    END IF;
  END write_byte;

PROCEDURE ClearLeafNodes IS
  pc           : integer_32;  -- previous code
  act_max_code : integer_32;  -- max code to be searched for leaf nodes

BEGIN
  act_max_code := next_free - 1;
  FOR i IN  Integer_32'(257) .. act_max_code  LOOP
    previous_code( i ):=
      integer_32( unsigned_32( previous_code(i) ) OR  16#8000#);
  END LOOP;

  FOR i IN  Integer_32'(257) .. act_max_code  LOOP
    pc:= integer_32( unsigned_32( previous_code(i) ) AND NOT 16#8000#);
    IF  pc > 256 THEN
      previous_code(pc) :=
        integer_32( unsigned_32( previous_code(pc) ) AND NOT 16#8000#);
    END IF;
  END LOOP;

  -- Build new free list
  pc := - 1;
  next_free := - 1;
  FOR  i  IN   Integer_32'(257).. act_max_code  LOOP

    -- Either free before or marked now
    IF integer_32( unsigned_32( previous_code(i) ) AND 16#C000#)  /= 0 THEN
      -- Link last item to this item
      IF pc /= -1 THEN
        previous_code( pc ) := -i;
      ELSE
        next_free := i;
      END IF;
      pc := i;
    END IF;

  END LOOP;

  IF  pc /= - 1 THEN
    previous_code ( pc ) := - act_max_code - 1;
  END IF;

END ClearLeafNodes;

procedure unshrink is

    incode       : integer_32;       -- code read in
    lastincode   : integer_32;       -- last code read in
    lastoutcode  : Byte;             -- last code emitted
    code_size    : integer;          -- Actual code size
    stack_ptr    : integer;          -- Stackpointer
    new_code     : integer_32;       -- Save new code read
    code_mask    : unsigned_32;      -- mask for coding

    bits_to_read : unsigned_32;

BEGIN
  previous_code:= (others=> 0);
  actual_code  := (others=> 0);
  stack        := (others=> 0);
  writebuf     := (others=> 0);

  IF  compsize = unsigned_32'last THEN  -- Compressed Size was not in header!
    raise Not_Supported;
  END IF;

  inpos := 0;            -- Input buffer position
  readpos := - 1;        -- Nothing read

  -- initialize window, bit buffer

  w := 0;
  k := 0;
  b := 0;

  -- initialize free codes list

  FOR  i  IN   Integer_32'(257).. max_code  LOOP
    previous_code(i) := - (i+1);
  END LOOP;

  next_free := 257;
  stack_ptr := max_stack;
  write_ptr := 0;
  code_size := initial_code_size;
  code_mask := mask_bits( natural(code_size) );

  Need_bits ( code_size );
  incode := integer_32( b AND code_mask );
  Dump_bits ( code_size );

  lastincode  := incode;
  lastoutcode := Byte( incode );
  write_byte ( lastoutcode );

  bits_to_read := unsigned_32(8 * compsize - unsigned_32(code_size));

  WHILE  bits_to_read >= unsigned_32(code_size)  LOOP

    Need_bits ( code_size );
    incode := integer_32(b  AND  code_mask);
    Dump_bits ( code_size );
    bits_to_read:= bits_to_read - unsigned_32(code_size);

    IF  incode = 256 THEN               -- Special code
      Need_bits ( code_size );
      incode := integer_32(b  AND  code_mask);
      Dump_bits ( code_size );
      bits_to_read:= bits_to_read - unsigned_32(code_size);

      CASE  incode  IS
        WHEN 1 =>
          code_size:= code_size + 1;
          IF  code_size > final_code_size THEN
            raise Zip_file_error;
          END IF;
          code_mask := mask_bits( natural(code_size) );

        WHEN 2 =>     ClearLeafNodes;
        WHEN others=> raise Zip_file_error;
      END CASE;

    ELSE
      new_code := incode;
      IF  incode < 256 THEN          --{Simple char}
        lastoutcode :=  Byte( incode );
        write_byte ( lastoutcode );
      ELSE
        IF  previous_code( incode ) < 0 THEN
          stack( stack_ptr ):= lastoutcode;
          stack_ptr:= stack_ptr -1;
          incode := lastincode;
        END IF;
        WHILE  incode > 256  LOOP
          stack( stack_ptr ):= actual_code( incode );
          stack_ptr:= stack_ptr -1;
          incode := previous_code( incode );
        END LOOP;

        lastoutcode :=  Byte( incode );
        write_byte ( lastoutcode );

        FOR  i  IN  stack_ptr+1.. max_stack  LOOP
          write_byte( stack( i ) );
        END LOOP;

        stack_ptr := max_stack;
      END IF;
      incode := next_free;
      IF  incode <= max_code THEN
        next_free := - previous_code( incode );   -- Next node in free list
        previous_code( incode ) := lastincode;
        actual_code  ( incode ) := lastoutcode;
      END IF;
      lastincode := new_code;
    END IF;
  END LOOP;

  BEGIN
    unshrink_flush;
  EXCEPTION
    when others=> raise Write_error;
  END;

END unshrink;

end UnZ_Unsh;
