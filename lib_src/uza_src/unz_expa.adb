with Interfaces;                        use Interfaces;
--with Text_IO;
with Unchecked_Deallocation;
with UnZ_Glob;                          use UnZ_Glob;
with UnZ_Tabl;                          use UnZ_Tabl;
with UnZ_IO;                            use UnZ_IO;

package body UnZ_Expa is

  DLE: CONSTANT:= 144;

  TYPE f_array IS  ARRAY (0..255,0..63 ) OF unsigned_32;
  TYPE pf_array IS ACCESS f_array;
  procedure  Dispose is new Unchecked_Deallocation( f_array, pf_array );

  followers: pf_array;
  Slen: ARRAY (0..255 ) OF unsigned_32;

  Slide_limit: constant:= WSize;  -- Was: 16#4000#;

  L_table: constant array(0..4) of unsigned_32:=
     (0,16#7f#,16#3f#,16#1f#,16#0f#);

  D_shift: constant array(0..4) of integer:=
     (0,16#07#,16#06#,16#05#,16#04#);

  D_mask:  constant array(0..4) of unsigned_32:=
     (0,16#01#,16#03#,16#07#,16#0f#);

  B_table: constant array(0..255) of integer:=

(8, 1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 5,
 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6,
 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7,
 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
 8, 8, 8, 8);


-- Function LoadFollowers

PROCEDURE LoadFollowers IS
  BEGIN
    FOR x IN REVERSE 0..255 LOOP
      Need_bits(6);
      Slen(x):=b  AND  mask_bits(6);
      Dump_bits(6);
      FOR  i IN  0 .. integer(Slen(x))-1  LOOP
        Need_bits(8);
        followers(x,i):= b  AND  mask_bits(8);
        Dump_bits(8);
      END LOOP;
    END LOOP;
  END LoadFollowers;

--  procedure Unreduce: expand probabilistically reduced data

procedure Unreduce( factor: integer ) is
    lchar   : integer;
    nchar   : unsigned_32;
    ExState : integer;
    v       : unsigned_32:= 0;
    len     : unsigned_32:= 0;
    s: file_size_type:= uncompsize;  -- number of bytes left to decompress
    w: integer;                      -- position in output window slide
    unflushed: boolean:= true;       -- true while slide unflushed
    d, d1, e   : integer;
    n          : unsigned_32;
    mask_bits8 : unsigned_32;
    bitsneeded : integer;
    follower   : unsigned_32;

BEGIN
  -- Initialize window, bit buffer
  w := 0;
  k := 0;
  b := 0;

  inpos  :=  0;         -- Input buffer position
  readpos:= -1;         -- Nothing read

  Zip_EOF:= False;

  lchar:=0;

  ExState:=0;

  mask_bits8:= mask_bits(8);

   --{:=pointer(@slide(Slide_limit));} ??!

  followers:= New f_array'(others=>(others=> 0));

  LoadFollowers;
  WHILE s > 0 AND THEN NOT Zip_EOF LOOP
    IF Slen(lchar)=0 THEN
      Need_bits(8);
      nchar:=b  AND  mask_bits8;
      Dump_bits(8);
    ELSE
      Need_bits(1);
      nchar:=b  AND  1;
      Dump_bits(1);
      IF  nchar/=0  THEN
        Need_bits(8);
        nchar:=b  AND  mask_bits8;
        Dump_bits(8);
      ELSE
        bitsneeded:= B_table( integer(Slen(lchar)));
        Need_bits(bitsneeded);
        follower:=b  AND  mask_bits(bitsneeded);
        Dump_bits(bitsneeded);
        nchar:= followers(lchar, integer(follower));
      END IF;
    END IF;

    -- Expand the resulting byte
    CASE  ExState  IS
      WHEN 0=>
          IF  nchar /= DLE THEN
            s:= s - 1;
            slide(w):= Byte(nchar);
            w:= w + 1;
            IF  w=Slide_limit THEN
              flush(w);
              w:=0;
              unflushed:= false;
            END IF;
          ELSE
            ExState:=1;
          END IF;

      WHEN 1=>
          IF  nchar /= 0 THEN
            V:= nchar;
            Len:= V  AND  L_table(factor);
            IF  Len=L_table(factor) THEN
              ExState:=2;
            ELSE
              ExState:=3;
            END IF;
          ELSE
            s:= s - 1;
            slide(w):= Byte(DLE);
            w:= w + 1;
            IF  w=Slide_limit THEN
              flush(w);
              w:= 0;
              unflushed:= false;
            END IF;
            ExState:=0;
          END IF;

      WHEN 2=>
          len:= len + nchar;
          ExState:=3;

      WHEN 3=>
          n:= len + 3;
           -- n: zu schreibende Bytes
           -- d: von hier kopieren
           -- e: zu kopierende Bytes
          d:= w - integer(((shift_right(V , D_shift(factor))  AND
                          D_mask(factor)) * (2**8)) + nchar + 1);
          s:= s - n;

          LOOP
            d:= d mod Slide_limit;
            IF  d > w THEN d1:=d; ELSE d1:=w; END IF;
            e:= Slide_limit-d1;
            IF  e>integer(n) THEN e:=integer(n); END IF;
            n:= n - unsigned_32(e);

            IF  unflushed  AND then w <= d THEN
              slide( w .. w+e-1 ):= (others=> 0);
              w:= w + e;
              d:= d + e;
            ELSIF abs(w - d) < e THEN
                LOOP
                  slide(w):=slide(d);
                  w:= w + 1; d:= d + 1; e:= e - 1;
                  EXIT WHEN  e=0;
                END LOOP;
            ELSE
                slide( w .. w+e-1 ):= slide( d .. d+e-1);
                w:= w + e;
                d:= d + e;
            END IF;
            IF  w=Slide_limit THEN
              flush(w);
              w:=0;
              unflushed:= false;
            END IF;
            EXIT WHEN  n=0;
          END LOOP;

          ExState:=0;

      when others => null;

    END CASE;

    lchar:= integer(nchar);    -- store character for next iteration
  END LOOP;

  Flush( w );   -- Flush out slide

  Dispose(followers);
END Unreduce;

end UnZ_Expa;
