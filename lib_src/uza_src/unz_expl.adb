with Interfaces;                        use Interfaces;
with Text_IO;
with UnZ_Glob;                          use UnZ_Glob;
with UnZ_Huft;                          use UnZ_Huft;
with UnZ_Tabl;                          use UnZ_Tabl;
with UnZ_IO;                            use UnZ_IO;

package body UnZ_Expl is

-- Note from Pascal source:
-- C code by info-zip group, translated to pascal by Christian Ghisler
-- based on unz51g.zip

-- ******************************** read in tree ****************************

procedure Get_tree ( l: out unsigned_16_array ) is
  i, k, j, b : unsigned_32;
  n          : unsigned_32:= l'length;
  l_idx      : integer    := l'first;
  bytebuf    : byte;

BEGIN
  if trace then Text_IO.Put_Line("Begin UnZ_Expl.Get_tree"); end if;

  Read_byte ( bytebuf );
  i := unsigned_32(bytebuf) + 1;
  k := 0;

  LOOP
    Read_byte ( bytebuf );
    j := unsigned_32(bytebuf);
    b :=   ( j  AND  16#0F# ) + 1;
    j := ( ( j  AND  16#F0# ) / 16 ) + 1;
    IF  k + j > n THEN
      raise Zip_file_error;
    END IF;

    LOOP
      l(l_idx) := unsigned_16(b);
      l_idx:= l_idx + 1;
      k:= k + 1;
      j:= j - 1;
      EXIT WHEN  j = 0;
    END LOOP;

    i:= i - 1;
    EXIT WHEN  i = 0;
  END LOOP;

  IF  k /= n THEN raise Zip_file_error; END IF;

  if trace then Text_IO.Put_Line("End   UnZ_Expl.Get_tree"); end if;
END Get_tree;

-- **************** exploding, generic method with 3 trees ******************

generic
  needed: integer;
  mask:   unsigned_32;
procedure explode_lit ( tb, tl, td : p_Table_list; bb, bl, bd : integer );

procedure explode_lit ( tb, tl, td : p_Table_list; bb, bl, bd : integer ) IS
    s       : unsigned_32;
    e, n, d : integer;

    w : integer:= 0;
    ct : p_HufT_table; -- current table
    ci : natural;      -- current index
    mb, ml, md : unsigned_32;
    unflushed: boolean:= true;       -- true while slide unflushed

BEGIN
  if trace then Text_IO.Put_Line("Begin Explode_lit"); end if;

  b := 0;
  k := 0;
  mb := mask_bits ( bb );
  ml := mask_bits ( bl );
  md := mask_bits ( bd );
  s := uncompsize;
  WHILE  s > 0  AND  NOT  Zip_EOF  LOOP
    Need_bits ( 1 );
    IF  ( b  AND  1 ) /= 0 THEN  -- Litteral
      Dump_bits( 1 );
      s:= s - 1;
      Need_bits( bb );
      ct:= tb.table;
      ci:= integer( ( NOT b ) AND  mb );

      LOOP
         e := integer( ct(ci).e );
         exit when e <= 16;

         IF  e = 99 THEN raise Zip_file_error; END IF;

         Dump_bits( integer( ct(ci).b ) );
         e:= e - 16;
         Need_bits ( e );
         ct:= ct(ci).nxt;
         ci:= integer( ( NOT b )  AND  mask_bits ( e ) );
      END LOOP;

      Dump_bits ( integer(ct(ci).b) );
      slide ( w ) :=  Byte ( ct(ci).n );
      w:= w + 1;
      IF  w = WSIZE THEN
        flush ( w );
        w := 0;
        unflushed:= false;
      END IF;
    ELSE
      Dump_bits ( 1 );
      Need_bits ( needed );
      d := integer(b  AND  mask);
      Dump_bits ( needed );
      Need_bits ( bd );
      ct := td.table;
      ci := integer( ( NOT b ) AND  md );

      LOOP
        e := integer(ct(ci).e);
        EXIT WHEN  e <= 16;

        IF  e = 99 THEN raise Zip_file_error; END IF;

        Dump_bits ( integer(ct(ci).b) );
        e:= e - 16;
        Need_bits ( e );
        ct := ct(ci).nxt;
        ci := integer( ( NOT b )  AND  mask_bits ( e ) );
      END LOOP;

      Dump_bits ( integer( ct(ci).b ) );

      d := w - d - integer( ct(ci).n );
      Need_bits ( bl );
      ct := tl.table;
      ci := integer( ( NOT b ) AND  ml );

      LOOP
        e := integer(ct(ci).e);
        EXIT WHEN  e <= 16;

        IF  e = 99 THEN raise Zip_file_error; END IF;

        Dump_bits ( integer( ct(ci).b ) );
        e:= e - 16;
        Need_bits ( e );
        ct := ct(ci).nxt;
        ci := integer( ( NOT b )  AND  mask_bits ( e ) );
      END LOOP;

      Dump_bits ( integer( ct(ci).b ) );

      n := integer( ct(ci).n );
      IF  e /= 0 THEN
        Need_bits ( 8 );
        n:= n + integer( b  AND  16#FF# );
        Dump_bits ( 8 );
      END IF;
      s:= s - unsigned_32(n);
      LOOP

        d := d  mod WSIZE;
        IF  d > w THEN e:= WSIZE - d; ELSE e:= WSIZE - w; END IF;
        IF  e > n THEN e := n; END IF;
        n:= n - e;
        IF unflushed AND THEN w <= d  THEN
          slide( w .. w+e-1 ):= (others=> 0);
          w:= w + e;
          d:= d + e;
        ELSIF  w - d >= e  THEN
          slide( w .. w+e-1 ):= slide( d .. d+e-1 );
          w:= w + e;
          d:= d + e;
        ELSE
         LOOP

          slide ( w ) := slide ( d );
          w:= w + 1;
          d:= d + 1;
          e:= e - 1;
          EXIT WHEN  e = 0;
         END LOOP;
        END IF;

        IF  w = WSIZE THEN
          flush ( w );
          w := 0;
          unflushed:= false;
        END IF;
        EXIT WHEN  n = 0;
       END LOOP;

    END IF;
  END LOOP;

  Flush ( w );
  IF Zip_EOF THEN raise Read_Error; END IF;

  if trace then Text_IO.Put_Line("End   Explode_lit"); end if;
END explode_lit;

-- **************** exploding, method: 8k slide, 3 trees *****************

procedure explode_lit8 is new explode_lit( 7, 16#7F# );

-- **************** exploding, method: 4k slide, 3 trees *****************

procedure explode_lit4 is new explode_lit( 6, 16#3F# );

-- **************** exploding, generic method with 2 trees ***************

generic
  needed: integer;
  mask:   unsigned_32;
procedure explode_nolit ( tl, td : p_Table_list;bl, bd : integer );

procedure explode_nolit ( tl, td : p_Table_list;bl, bd : integer ) is
    s       : unsigned_32;
    e, n, d : integer;
    w : integer:= 0;
    ct : p_HufT_table; -- current table
    ci : natural;      -- current index
    ml, md : unsigned_32;
    unflushed: boolean:= true;       -- true while slide unflushed

BEGIN
  if trace then Text_IO.Put_Line("Begin Explode_nolit"); end if;

  b := 0;
  k := 0;
  ml := mask_bits ( bl );
  md := mask_bits ( bd );
  s := uncompsize;
  WHILE  s > 0  AND  NOT Zip_EOF  LOOP
    Need_bits ( 1 );
    IF  ( b  AND  1 ) /= 0 THEN  -- Litteral
      Dump_bits ( 1 );
      s:= s - 1;
      Need_bits ( 8 );
      slide ( w ) :=  Byte( b );
      w:= w + 1;
      IF  w = WSIZE THEN
        Flush( w );
        w := 0;
        unflushed:= false;
      END IF;
      Dump_bits ( 8 );
    ELSE
      Dump_bits ( 1 );
      Need_bits ( needed );
      d := integer(b AND mask);
      Dump_bits ( needed );
      Need_bits ( bd );
      ct := td.table;
      ci := integer( ( NOT b )  AND  md );

      LOOP
        e := integer(ct(ci).e);
        EXIT WHEN  e <= 16;

        IF  e = 99 THEN raise Zip_file_error; END IF;

        Dump_bits ( integer(ct(ci).b) );
        e:= e - 16;
        Need_bits ( e );
        ct := ct(ci).nxt;
        ci := integer( ( NOT b )  AND  mask_bits ( e ) );
      END LOOP;

      Dump_bits ( integer(ct(ci).b) );

      d := w - d - integer(ct(ci).n);
      Need_bits ( bl );
      ct := tl.table;
      ci := integer( ( NOT b ) AND  ml );

      LOOP
        e := integer(ct(ci).e);
        EXIT WHEN  e <= 16;

        IF  e = 99 THEN raise Zip_file_error; END IF;

        Dump_bits ( integer(ct(ci).b) );
        e:= e - 16;
        Need_bits ( e );
        ct := ct(ci).nxt;
        ci := integer( ( NOT b )  AND  mask_bits ( e ) );
      END LOOP;

      Dump_bits ( integer(ct(ci).b) );

      n := integer(ct(ci).n);
      IF  e /= 0 THEN
        Need_bits ( 8 );
        n:= n + integer( b  AND  16#FF# );
        Dump_bits ( 8 );
      END IF;
      s:= s - unsigned_32(n);
      LOOP

        d := d mod WSIZE;
        IF  d > w THEN e:= WSIZE - d ; ELSE e:= WSIZE - w; END IF;
        IF  e > n THEN e:= n; END IF;
        n:= n - e;
        IF unflushed AND THEN  w <= d  THEN
          slide( w .. w+e-1 ):= (others=> 0);
          w:= w + e;
          d:= d + e;
        ELSIF  w - d >= e THEN
          slide( w .. w+e-1 ):= slide ( d .. d+e-1 );
          w:= w + e;
          d:= d + e;
        ELSE
          LOOP
            slide ( w ) := slide ( d );
            w:= w + 1;
            d:= d + 1;
            e:= e - 1;
            EXIT WHEN  e = 0;
          END LOOP;
        END IF;

        IF  w = WSIZE THEN
          Flush ( w );
          w := 0;
          unflushed:= false;
        END IF;
        EXIT WHEN  n = 0;

       END LOOP;

    END IF;
  END LOOP;

  Flush ( w );
  IF Zip_EOF THEN raise Read_Error; end if;

  if trace then Text_IO.Put_Line("End   Explode_nolit"); end if;

END explode_nolit;

-- ****************** exploding, method: 8k slide, 2 trees ******************

procedure explode_nolit8 is new explode_nolit( 7, 16#7F# );

-- ****************** exploding, method: 4k slide, 2 trees ******************
procedure explode_nolit4 is new explode_nolit( 6, 16#3F# );

-- ****************************** explode *********************************

procedure Explode( literal_tree, slide_8_KB: boolean ) is

    tb, tl, td : p_Table_list;
    bb, bl, bd : integer;
    l:  unsigned_16_array( 0..255 );

BEGIN
  inpos  :=  0;         -- Input buffer position
  readpos:= -1;         -- Nothing read

  bl := 7;
  IF  compsize > 200000 THEN bd := 8; ELSE bd := 7; END IF;

  IF literal_tree THEN
    bb := 9;
    Get_tree ( l( 0..255 ) );
    begin
      huft_build ( l( 0..255 ), 256, cp_empty, cp_empty, tb, bb );
    exception
      when  huft_incomplete => huft_free (tb); raise Zip_file_error;
      when  others => raise Zip_file_error;
    end;

    begin
      Get_tree ( l( 0..63 ) );
    exception
      when others=>  huft_free( tb ); raise Zip_file_error;
    end;

    begin
      huft_build ( l( 0..63 ), 0, cplen3, extra, tl, bl );
    exception
      when  huft_incomplete =>
              huft_free(tl); huft_free(tb); raise Zip_file_error;
      when  others =>  huft_free (tb); raise Zip_file_error;
    end;

    begin
      Get_tree ( l( 0..63 ) );
    exception
      when others=>  huft_free( tb ); huft_free( tl ); raise Zip_file_error;
    end;

    begin
      IF slide_8_KB THEN
        huft_build ( l( 0..63 ), 0, cpdist8, extra, td, bd );
        explode_lit8 ( tb, tl, td, bb, bl, bd );
      ELSE
        huft_build ( l( 0..63 ), 0, cpdist4, extra, td, bd );
        explode_lit4 ( tb, tl, td, bb, bl, bd );
      END IF;
    exception
      when  huft_incomplete =>
              huft_free(td);
              huft_free(tl);
              huft_free(tb); raise Zip_file_error;
--       when  others =>
--               huft_free(tl);
--               huft_free(tb); raise Zip_file_error;
    end;

    huft_free ( td );
    huft_free ( tl );
    huft_free ( tb );

  ELSE         -- No literal tree

    begin
      Get_tree ( l( 0..63 ) );
    exception
      when others=>  raise Zip_file_error;
    end;

    begin
      huft_build ( l( 0..63 ), 0, cplen2, extra, tl, bl );
    exception
      when huft_incomplete => huft_free(tl); raise Zip_file_error;
      when others => raise Zip_file_error;
    end;

    begin
      Get_tree ( l( 0..63 ) );
    exception
      when others=> huft_free (tl); raise Zip_file_error;
    end;

    begin
      IF slide_8_KB THEN
        huft_build ( l( 0..63 ), 0, cpdist8, extra, td, bd );
        explode_nolit8 ( tl, td, bl, bd );
      ELSE
        huft_build ( l( 0..63 ), 0, cpdist4, extra, td, bd );
        explode_nolit4 ( tl, td, bl, bd );
      END IF;
    exception
        when huft_incomplete =>
           huft_free(td);
           huft_free(tl); raise Zip_file_error;
        when others=>
           huft_free(tl); raise Zip_file_error;
    end;

    huft_free ( td );
    huft_free ( tl );
  END IF;

END Explode;

end UnZ_Expl;
