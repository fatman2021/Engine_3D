-- UnZip-Ada
------------
-- Definiton and interpretation of PKZIP info structures (read appnote.txt)

with Interfaces;                        use Interfaces;
with UnZ_Glob;                          use UnZ_Glob;

package UnZ_Head is

    -- PKZIP local file header, in front of every file in archive

    type t_localheader is record
      sig_P, sig_K       : character;
      sig_3, sig_4       : byte;
      needed_extract_ver : unsigned_16;
      bit_flag,
      zip_type           : unsigned_16;
      file_timedate      : unsigned_32;
      crc_32             : unsigned_32;
      compress_size,
      uncompress_size    : unsigned_32;
      filename_len,
      extra_field_len    : unsigned_16;
    end record;

    bad_local_header: exception;

    -- PKZIP file header, as in central directory

    type t_header is record
      sig_P, sig_K       : character;
      sig_1, sig_2       : byte;
      made_by_ver        : unsigned_16;
      needed_extract_ver : unsigned_16;
      bit_flag           : unsigned_16;
      zip_type           : unsigned_16;
      file_timedate      : unsigned_32;
      crc_32             : unsigned_32;
      compress_size      : unsigned_32;
      uncompress_size    : unsigned_32;
      filename_len       : unsigned_16;
      extra_field_len    : unsigned_16;
      comment_len        : unsigned_16;
      disk_number_start  : unsigned_16;
      internal_attrib    : unsigned_16;
      external_attrib    : unsigned_32;
      local_header_offset: unsigned_32;
    end record;

    bad_header: exception;

    -- PKZIP end-of-central-directory

    type t_end is record
      sig_P, sig_K       : character;
      sig_5, sig_6       : byte;
      disknum            : unsigned_16;  -- disk spanning unsupported here!
      disknum_with_start : unsigned_16;
      disk_total_entries : unsigned_16;
      total_entries      : unsigned_16;
      central_dir_size   : unsigned_32;
      central_dir_offset : unsigned_32;
      main_comment_len   : unsigned_16;
    end record;

    bad_end: exception;

  procedure Interpret(
              local_header_buffer : in  IO_buf;
              local_header        : out t_localheader );

  procedure Interpret(
              header_buffer : in  IO_buf;
              header        : out t_header );

  procedure Interpret(
              end_buffer : in  IO_buf;
              the_end    : out t_end );

end UnZ_Head;
