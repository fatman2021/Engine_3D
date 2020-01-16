-- UnZip-Ada
------------
-- Buffering, I/O

with Interfaces;                        use Interfaces;
with Unzip;
with UnZ_Glob;                          use UnZ_Glob;
with Direct_IO;

package UnZ_IO is

  package Byte_IO is new  Direct_IO( byte );

  infile:  Byte_IO.file_type;
  outfile: Byte_IO.file_type;

  current_feedback: Unzip.feedback_proc:= null;

  procedure Open_infile( in_name: String );

  -- **** Read byte, only used by explode ****
  procedure Read_byte ( bt : out byte );

  -- *********** Read at least n bits into the global variable b *************
  procedure Need_bits ( n: natural );

  -- ***************** Dump n bits no longer needed from global variable b ***
  procedure Dump_bits ( n: natural );

  -- ********************* Flush w bytes directly from slide to file *********
  procedure Flush ( w: natural );

  pragma Inline( Read_byte, Need_bits, Dump_bits );

  -- This is to read data in the "Data descriptor" after zipped data:
  function Get_via_need_dump return unsigned_32;

end UnZ_IO;
