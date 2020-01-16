-- UnZip-Ada
------------
-- Global constants, types and variables
-- NB: in this form, obviously unsafe for concurrent tasking!

-- Note from Pascal source:
-- C code by info-zip group, translated to pascal by Christian Ghisler
-- based on unz51g.zip

with Interfaces;                        use Interfaces;
with Ada.Streams;                       use Ada.Streams;

package UnZ_Glob is

  trace : constant boolean:= false; -- display primitive tracing with Text_IO;

-- I/O Buffers sizes

--   Size of input buffer       (orig: 16#1000# B =  4 KB)
  inbuf_size : constant:= 16#8000#;

--   Size of sliding dictionary (orig: 16#8000# B = 32 KB)
  wsize      : constant:= 16#10000#;

  subtype  byte is unsigned_8;
  type pbyte is access byte;

  type p_unsigned_16 is access unsigned_16;

  type  IO_buf is ARRAY ( integer range <> ) OF byte;
  type p_IO_buf is access IO_buf;

  type unsigned_16_array is array(integer range <>) of unsigned_16;

  slide : IO_Buf( 0..wsize );     -- Sliding dictionary for unzipping

 -- Input buffer
  inbufsm1 : constant:= inbuf_size - 1;

  inbuf: IO_buf( 0..inbufsm1 );

  inpos, readpos: integer;  -- position in input buffer, pos. read from file

  w : integer;              -- Current Position in slide
  b : unsigned_32;          -- Bit Buffer
  k : integer;              -- Bits in bit buffer

-- Data sizes in archive
  subtype file_size_type is unsigned_32; -- should be typed with xxx_IO.Count

  compsize,            -- compressed size of file
  reachedsize,         -- number of bytes read from zipfile
  uncompsize,          -- uncompressed size of file
  effective_writes :   -- count of effective bytes written (for feedback)
    file_size_type;

  crc32val : unsigned_32;  -- crc calculated from data
  Zip_EOF  : boolean;      -- read over end of zip section for this file

  type t_write_mode is
    ( write_to_file,
      write_to_memory,
      just_test);

  write_mode : t_write_mode;

  type p_Stream_Element_Array is access all Stream_Element_Array;
  uncompressed_memory : p_Stream_Element_Array;
  uncompressed_index  : Stream_Element_Offset;


-- Errors

  CRC_Error, Write_error, Read_error,
  Zip_file_error,
  Zip_file_open_error,
  File_name_not_found,
  Duplicate_name,
  User_abort,
  Not_Supported,
  Encrypted,
  Internal_Error : exception;

-- Codes for methods

  stored:    constant:= 0;
  shrunk:    constant:= 1;
  reduced_1: constant:= 2;
  reduced_2: constant:= 3;
  reduced_3: constant:= 4;
  reduced_4: constant:= 5;
  imploded:  constant:= 6;
  deflated:  constant:= 8;

  SupportedMethods: constant:=
    2 ** stored    + 2 ** shrunk    +
    2 ** reduced_1 + 2 ** reduced_2 +
    2 ** reduced_3 + 2 ** reduced_4 +
    2 ** imploded  +
    2 ** deflated;

end UnZ_Glob;
