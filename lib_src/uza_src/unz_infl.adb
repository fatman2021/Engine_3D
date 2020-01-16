with Interfaces;                        use Interfaces;
with Text_IO;
with UnZ_Glob;                          use UnZ_Glob;
with UnZ_Huft;                          use UnZ_Huft;
with UnZ_Tabl;                          use UnZ_Tabl;
with UnZ_IO;                            use UnZ_IO;

package body UnZ_Infl is

-- Note from Pascal source:
-- C code by info-zip group, translated to pascal by Christian Ghisler
-- based on unz51g.zip

procedure Inflate_codes ( tl, td: p_Table_list; bl, bd: integer ) is

    cte  : p_Huft;       -- current table element
    n, d : integer;      -- length and index for copy
    e    : integer;      -- table entry flag/number of extra bits
    w    : integer:= UnZ_Glob.w;  -- local variable for slide index w

    ml   : unsigned_32:= mask_bits ( bl );    -- masks for bl bits
    md   : unsigned_32:= mask_bits ( bd );    -- masks for bd bits

BEGIN
  if trace then Text_IO.Put_Line("Begin Inflate_codes"); end if;

  -- inflate the coded data
  WHILE  not Zip_EOF  LOOP
    Need_bits ( bl );
    cte:= tl.table(integer( b  AND  ml ))'access;

    LOOP
      e := integer(cte.e);
      exit when e <= 16;
      if e = 99 THEN raise Zip_file_error; END IF;

      -- then it's a literal
      Dump_bits ( integer(cte.b) );
      e:= e - 16;
      Need_bits ( e );
      cte := cte.nxt(integer( b  AND  mask_bits ( e ) ))'access;
    END LOOP;

    Dump_bits ( integer(cte.b) );

    case e is
      when 16 =>
        slide ( w ) :=  Byte( cte.n );
        w:= w + 1;
        IF  w = WSIZE THEN
          Flush( w );
          w := 0;
        END IF;

      when 15 => -- End of block (EOB)
        if trace then Text_IO.Put_Line("Exit  Inflate_codes, e=15 EOB"); end if;
        UnZ_Glob.w:= w;
        return;

      when others => -- it's a length
        Need_bits ( e );                 -- get length of block to copy
        n := cte.n + integer( b  AND  mask_bits ( e ) );
        Dump_bits ( e );

        Need_bits ( bd );                -- decode distance of block to copy

        -- if td=null then raise Internal_Error; end if;

        cte := td.table(integer( b  AND  md ))'access;

        LOOP
          e := integer(cte.e);
          exit when e <= 16;

          IF  e = 99 THEN raise Zip_file_error; END IF;

          Dump_bits ( integer(cte.b) );
          e:= e - 16;
          Need_bits ( e );
          cte := cte.nxt(integer( b  AND  mask_bits ( e ) ))'access;
        END LOOP;

        Dump_bits ( integer(cte.b) );
        Need_bits ( e );
        d := w - cte.n - integer( b  AND  mask_bits ( e ) );
        Dump_bits ( e );

        -- Do the copy
        LOOP
          d := d mod WSIZE; -- d becomes [0..wsize-1] (optimizer "and"s)

          IF  d > w THEN e := WSIZE - d; ELSE e := WSIZE - w; END IF;
          -- e always > 0 here

          IF  e > n THEN e := n; END IF;
          n:= n - e;

          IF  abs(w - d) >= e  THEN
            if false then Text_IO.Put_Line("(Inflate_codes) copy e=" &
              integer'image(e) & " elements from w=" &
              integer'image(w) & " in block ");
            end if;
            slide( w..w+e-1 ):= slide ( d..d+e-1 );
            w:= w + e;
            d:= d + e;
          ELSE
            if false then Text_IO.Put_Line("(Inflate_codes) copy e=" &
              integer'image(e) & " elements from w=" &
              integer'image(w) & " 1-per-1 ");
            end if;
            LOOP
              slide( w ) := slide( d );
              w:= w + 1;
              d:= d + 1;
              e:= e - 1;
              EXIT WHEN e = 0;
            END LOOP;
          END IF;
          IF  w = WSIZE THEN
            Flush ( w );
            w := 0;
          END IF;
          EXIT WHEN  n = 0;
        END LOOP;

    end case;
  END LOOP;

  UnZ_Glob.w:= w;

  if trace then Text_IO.Put_Line("End   Inflate_codes"); end if;
END Inflate_codes;

-- **************************** "decompress" stored block *******************

procedure Inflate_stored is
  n : integer;            -- number of bytes in block

BEGIN
  if trace then Text_IO.Put_Line("Begin Inflate_stored"); end if;
  -- Go to byte boundary
  n := k mod 8;
  Dump_bits ( n );
  -- get the length and its complement
  Need_bits ( 16 );
  n := integer(b AND 16#ffff#);
  Dump_bits ( 16 );
  Need_bits ( 16 );
  IF  n /= integer(( NOT b ) AND 16#ffff#) THEN raise Zip_file_error; end if;
  Dump_bits ( 16 );
  WHILE n > 0  AND then NOT Zip_EOF LOOP -- read and output the compressed data
    n:= n - 1;
    Need_bits ( 8 );
    slide ( w ) := Byte( b );
    w:= w + 1;
    IF  w = WSIZE THEN
      Flush( w );
      w := 0;
    END IF;
    Dump_bits ( 8 );
  END LOOP;
  if trace then Text_IO.Put_Line("End   Inflate_stored"); end if;
END Inflate_stored;

-- **************************** decompress fixed block **************************

procedure Inflate_fixed is
  tl,                        -- literal/length code table
  td : p_Table_list;            -- distance code table
  bl, bd : integer;          -- lookup bits for tl/bd

  -- length list for huft_build (literal table)
  l: unsigned_16_array( 0..287 ):=
           ( 0..143=> 8, 144..255=> 9, 256..279=> 7, 280..287=> 8);

BEGIN
  if trace then Text_IO.Put_Line("Begin Inflate_fixed"); end if;

  -- make a complete, but wrong code set
  bl := 7;

  huft_build( l, 257, cplens, cplext, tl, bl );

  l( 0..29 ):= ( 0..29=> 5);

  -- make an incomplete code set
  bd := 5;

  begin
    huft_build( l( 0..29 ), 0, cpdist, cpdext, td, bd );
  exception
    when huft_incomplete =>
      td:= last_list; -- out parameter has been zeroed
      if trace then Text_IO.Put_Line("td is incomplete, pointer=null: " &
                                     boolean'image(td=null));
      end if;
    when huft_out_of_memory | huft_error =>
      huft_free( tl ); raise Zip_file_error;
  end;

  Inflate_codes ( tl, td, bl, bd );

  huft_free ( tl );
  huft_free ( td );

  if trace then Text_IO.Put_Line("End   Inflate_fixed"); end if;
END Inflate_fixed;

-- ************************ decompress dynamic block ************************

procedure Inflate_dynamic is

    lbits : constant:= 9;
    dbits : constant:= 6;

    i : integer;                    -- temporary variables
    j : natural;
    l : natural;                    -- last length
    m : unsigned_32;                -- mask for bit length table
    n : natural;                    -- number of lengths to get

    tl,                             -- literal/length code tables
    td : p_Table_list;              -- distance code tables

    cte : p_Huft;             -- current table element

    bl, bd : integer;                  -- lookup bits for tl/bd
    nb : natural;  -- number of bit length codes
    nl : natural;  -- number of literal length codes
    nd : natural;  -- number of distance codes

  -- literal/length and distance code lengths
    ll_limit : constant := 288+32-1;
    ll       : unsigned_16_array( 0..ll_limit ):= (others=> 0);

BEGIN
  if trace then Text_IO.Put_Line("Begin Inflate_dynamic"); end if;

  -- Read in table lengths
  Need_bits ( 5 );
  nl := 257 + integer( b  AND  16#1f# );
  Dump_bits ( 5 );
  Need_bits ( 5 );
  nd :=   1 + integer( b  AND  16#1f# );
  Dump_bits ( 5 );
  Need_bits ( 4 );
  nb :=   4 + integer( b  AND  16#0f# );
  Dump_bits ( 4 );
  IF  nl > 288 or else nd > 32 THEN raise Zip_file_error; END IF;

  -- Read in bit-length-code lengths

  FOR j IN  0 .. nb - 1  LOOP
    Need_bits ( 3 );
    ll ( bit_order( j ) ) := unsigned_16(b  AND  7);
    Dump_bits ( 3 );
  END LOOP;

  FOR j IN nb .. 18  LOOP  ll( bit_order( j ) ) := 0; END LOOP;

  -- Build decoding table for trees--single level, 7 bit lookup
  bl := 7;
  begin
    huft_build ( ll( 0..18 ), 19, cp_empty, cp_empty, tl, bl );
  exception
    when huft_incomplete => huft_free(tl); raise Zip_file_error;
    when others => raise Zip_file_error;
  end;

  -- Read in literal and distance code lengths

  n := nl + nd;
  m := mask_bits ( bl );
  i := 0;
  l := 0;
  WHILE  i < n  LOOP
    Need_bits ( bl );
    cte := tl.table(integer( b  AND  m ))'access;

    Dump_bits ( integer(cte.b) );
    j := cte.n;

    case j is
      when 0..15 =>         -- length of code in bits (0..15)
        l:= j;                       -- save last length in l
        ll ( i ) := unsigned_16(l);
        i:= i + 1;

      when 16 =>          -- repeat last length 3 to 6 times
        Need_bits ( 2 );
        j := 3 + integer( b  AND  3 );
        Dump_bits ( 2 );
        IF  i + j > n THEN raise Zip_file_error; END IF;
        WHILE  j > 0  LOOP
          ll ( i ) := unsigned_16(l);
          i:= i + 1;
          j:= j - 1;
        END LOOP;

      when 17 =>          -- 3 to 10 zero length codes
        Need_bits ( 3 );
        j := 3 + integer( b  AND  7 );
        Dump_bits ( 3 );
        IF  i + j > n THEN raise Zip_file_error; END IF;
        WHILE  j > 0  LOOP
          ll ( i ) := 0;
          i:= i + 1;
          j:= j - 1;
        END LOOP;

        l := 0;

      when 18 =>          -- j = 18: 11 to 138 zero length codes
        Need_bits ( 7 );
        j := 11 + integer( b  AND  16#7f# );
        Dump_bits ( 7 );
        IF  i + j > n THEN raise Zip_file_error; end if;
        WHILE  j > 0  LOOP
          ll ( i ) := 0;
          i:= i + 1;
          j:= j - 1;
        END LOOP;

        l := 0;

      when others =>
          if trace then Text_IO.Put_Line("Illegal length code: " &
            integer'image(j));
          end if;

    end case;
  END LOOP;

  huft_free ( tl );        -- free decoding table for trees

  -- Build the decoding tables for literal/length codes
  bl := lbits;
  begin
    huft_build ( ll( 0..nl-1 ), 257, cplens, cplext, tl, bl );
  exception
    when huft_incomplete => huft_free(tl); raise Zip_file_error;
    when others => raise Zip_file_error;
  end;

  -- Build the decoding tables for distance codes
  bd := dbits;
  begin
    huft_build ( ll( nl..nl+nd-1 ), 0, cpdist, cpdext, td, bd );
  exception
    when huft_out_of_memory | huft_error =>
      huft_free(tl); raise Zip_file_error;
    when huft_incomplete =>
      if trace then Text_IO.Put_Line("PKZIP 1.93a bug workaround"); end if;
  end;

  -- Decompress until an end-of-block code

  Inflate_codes ( tl, td, bl, bd );
  huft_free ( tl );
  huft_free ( td );

  if trace then Text_IO.Put_Line("End   Inflate_dynamic"); end if;
END Inflate_dynamic;

-- **************************** decompress a block *************************

procedure Inflate_block( e: in out integer ) is

  t: unsigned_32;           -- block type

BEGIN
  Need_bits ( 1 );
  e := integer(b  AND  1);
  Dump_bits ( 1 );

  Need_bits ( 2 );
  t := b  AND  3;
  Dump_bits ( 2 );

  CASE  t  IS
    WHEN 0 =>     inflate_stored;
    WHEN 1 =>     inflate_fixed;
    WHEN 2 =>     inflate_dynamic;
    WHEN others=> raise Zip_file_error;  -- bad block type
  END CASE;

END Inflate_block;

-- *********************** decompress an inflated entry *********************

procedure Inflate is
    e: integer;           -- last block flag

BEGIN
  -- Initialize window, bit buffer
  w:= 0;
  k:= 0;
  b:= 0;

  inpos  :=  0;         -- Input buffer position
  readpos:= -1;         -- Nothing read

  -- decompress until the last block
  LOOP
    Inflate_block ( e );
    EXIT WHEN  e /= 0;
  END LOOP;

  Flush( w );   -- Flush out slide
  w:= 0;

END Inflate;

end UnZ_Infl;
