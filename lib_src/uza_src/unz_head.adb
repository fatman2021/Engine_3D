package body UnZ_Head is

  -- Get numbers with correct trucmuche endian, to ensure
  -- correct header loading on some non-Intel machines

  generic type number is mod <>; -- range <> in Ada83 version (fake Interfaces)
  function Intel_x86_number( b: io_buf ) return number;

  function Intel_x86_number( b: io_buf ) return number is
    n: number:= 0;
    begin
      for i in reverse b'range loop
        n:= n * 256 + number(b(i));
      end loop;
      return n;
    end Intel_x86_number;

   function Intel_nb is new Intel_x86_number( unsigned_16 );
   function Intel_nb is new Intel_x86_number( unsigned_32 );

  procedure Interpret(
              local_header_buffer : in  IO_buf;
              local_header        : out t_localheader ) is

    lhb: constant IO_buf( 1..30 ):=
           local_header_buffer(local_header_buffer'first ..
                               local_header_buffer'first + 29 );

    begin
      if character'val(integer(lhb(1))) /= 'P' or else
         character'val(integer(lhb(2))) /= 'K' or else
         lhb(3 .. 4) /= (3, 4) then
           raise bad_local_header;
      end if;

      local_header.needed_extract_ver:= Intel_nb( lhb( 5.. 6) );
      local_header.bit_flag:=           Intel_nb( lhb( 7.. 8) );
      local_header.zip_type:=           Intel_nb( lhb( 9..10) );
      local_header.file_timedate:=      Intel_nb( lhb(11..14) );
      local_header.crc_32:=             Intel_nb( lhb(15..18) );
      local_header.compress_size:=      Intel_nb( lhb(19..22) );
      local_header.uncompress_size:=    Intel_nb( lhb(23..26) );
      local_header.filename_len:=       Intel_nb( lhb(27..28) );
      local_header.extra_field_len:=    Intel_nb( lhb(29..30) );

    end Interpret;

  procedure Interpret(
              header_buffer : in  IO_buf;
              header        : out t_header ) is

    hb: constant IO_buf( 1..46 ):=
           header_buffer(header_buffer'first ..
                               header_buffer'first + 45 );

    begin
      if character'val(integer(hb(1))) /= 'P' or else
         character'val(integer(hb(2))) /= 'K' or else
         hb(3 .. 4) /= (1, 2) then
           raise bad_header;
      end if;

      header.made_by_ver:=          Intel_nb( hb( 5.. 6) );
      header.needed_extract_ver:=   Intel_nb( hb( 7.. 8) );

      header.bit_flag:=             Intel_nb( hb( 9..10) );
      header.zip_type:=             Intel_nb( hb(11..12) );

      header.filename_len:=         Intel_nb( hb(29..30) );
      header.extra_field_len:=      Intel_nb( hb(31..32) );
      header.comment_len:=          Intel_nb( hb(33..34) );

      header.local_header_offset:=  Intel_nb( hb(43..46) );

    end Interpret;

  procedure Interpret(
              end_buffer : in  IO_buf;
              the_end    : out t_end ) is

    eb: constant IO_buf( 1..22 ):=
               end_buffer(end_buffer'first .. end_buffer'first + 21 );

    begin
      if character'val(integer(eb(1))) /= 'P' or else
         character'val(integer(eb(2))) /= 'K' or else
         eb(3 .. 4) /= (5, 6) then
           raise bad_end;
      end if;

      the_end.disknum:=              Intel_nb( eb( 5.. 6) );
      the_end.disknum_with_start:=   Intel_nb( eb( 7.. 8) );
      the_end.disk_total_entries:=   Intel_nb( eb( 9..10) );
      the_end.total_entries:=        Intel_nb( eb(11..12) );
      the_end.central_dir_size:=     Intel_nb( eb(13..16) );
      the_end.central_dir_offset:=   Intel_nb( eb(17..20) );
      the_end.main_comment_len:=     Intel_nb( eb(21..22) );

    end Interpret;

end UnZ_Head;
