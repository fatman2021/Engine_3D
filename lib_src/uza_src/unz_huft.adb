with Text_IO;
with Unchecked_Deallocation;

package body UnZ_HufT is

-- Note from Pascal source:
-- C code by info-zip group, translated to pascal by Christian Ghisler
-- based on unz51g.zip

-- *************** free huffman tables starting with table where t points to

procedure  HufT_Free ( tl: in out p_Table_list ) is

  procedure  Dispose is new Unchecked_Deallocation( HufT_table, p_HufT_table );
  procedure  Dispose is new Unchecked_Deallocation( Table_list, p_Table_list );

  current: p_Table_list;

  BEGIN
    if trace then Text_IO.Put("[HufT_Free..."); end if;
    while tl /= null loop
      Dispose( tl.table ); -- destroy the Huffman table
      current:= tl;
      tl     := tl.next;
      Dispose( current );  -- destroy the current node
    end loop;
    if trace then Text_IO.Put_Line("finished]"); end if;
  END HufT_Free;

-- *********** build huffman table from code lengths given by array b

procedure HufT_Build ( b    : unsigned_16_array;
                       s    : integer;
                       d, e : copy_length_array;
                       tl   : in out p_Table_list;
                       m    : in out integer) is

   n: constant integer:= b'length;

   b_max  : constant:= 16;
   b_maxp1: constant:= b_max + 1;

   -- bit length count table
   c : ARRAY( 0 .. b_maxp1 ) OF integer:= (others=> 0);

   f   : integer;                    -- i repeats in table every f entries
   g   : integer;                    -- max. code length
   h   : integer:= -1;               -- table level
   i,                                -- counter, current code
   j   : integer;                    -- counter
   kcc : integer;                    -- number of bits in current code

   b_idx, c_idx,
   x_idx, v_idx: natural;            -- array indices

   current_table_ptr : p_HufT_table:= Null;
   current_node_ptr  : p_Table_list; -- current node for the current table
   new_node_ptr      : p_Table_list; -- new node for the new table
   first_table  : boolean:= true;

   new_entry: huft;                  -- table entry for structure assignment

   u : array( 0..b_max ) OF p_HufT_table;   -- table stack

   n_max : constant:= 288;
   -- values in order of bit length
   v : array( 0..n_max ) of integer:= (others=> 0);
   el_v, el_v_m_s: integer;

   w : natural:= 0;                        -- bits before this table

   x : ARRAY (  0..b_maxp1 ) OF integer;   -- bit offsets, then code stack

   -- l(h) bits in table of level h
   l : array( integer'(-1)..b_maxp1 ) of integer;

   y  : integer;                     -- number of dummy codes added
   z  : natural:= 0;                 -- number of entries in current table
   el : integer;                     -- length of eob code=code 256

   no_copy_length_array: constant boolean:= d'length=0 OR ELSE e'length=0;

  BEGIN
    if trace then Text_IO.Put("[HufT_Build..."); end if;
    tl:= null;

    IF n > 256 THEN             -- set length of EOB code, if any
      el := integer( b(256) );
    ELSE
      el := b_max;
    end if;

    -- Generate counts for each bit length

    b_idx:= b'first;
    for i in reverse 1..n loop
      IF b(b_idx) > b_max THEN
        m := 0;
        raise huft_error;
      END IF;
      c( integer(b(b_idx)) ):= c( integer(b(b_idx)) ) + 1;
      b_idx:= b_idx + 1;
    end loop;

    IF c(0) = n THEN
      m := 0;
      return; -- complete
    END IF;

    -- Find minimum and maximum length, bound m by those

    j := 1;
    WHILE j <= b_max and then c(j) = 0 LOOP j:= j + 1; END LOOP;
    kcc := j;
    IF m < j THEN m := j; end if;
    i := b_max;
    WHILE    i > 0   and then c(i) = 0 LOOP i:= i - 1; END LOOP;
    g := i;
    IF m > i THEN m := i; END IF;

    -- Adjust last length count to fill out codes, if needed

    y := integer(shift_left(unsigned_32'(1), j)); -- y:= 2 ** j;
    WHILE j < i LOOP
      y := y - c(j);
      IF y < 0 THEN
        raise huft_error;
      END IF;
      y:= y * 2;
      j:= j + 1;
    END LOOP;

    y:= y - c(i);
    IF y < 0 THEN
      raise huft_error;
    END IF;
    c(i):= c(i) + y;

    -- Generate starting offsets into the value table for each length

    x(1) := 0;
    j    := 0;
    c_idx:= c'first + 1;
    x_idx:= x'first + 2;

    for count in reverse 0..i-2 loop
      j:= j + c( c_idx );
      c_idx:= c_idx + 1;
      x( x_idx ) := j;
      x_idx:= x_idx + 1;
    end loop;

    -- Make table of values in order of bit length

    b_idx:= b'first;
    i:= 0;
    LOOP
      j := integer(b( b_idx));
      b_idx:= b_idx + 1;
      IF j /= 0 THEN
        v( x(j) ) := i;
        x(j):= x(j) + 1;
      END IF;
      i:= i + 1;
      exit when i >= n;
    END LOOP;

    -- Generate huffman codes and for each, make the table entries

    x(0) := 0;
    i := 0;
    v_idx:= v'first;
    l(-1) := 0;

    -- go through the bit lengths (kcc already is bits in shortest code)
    FOR k in kcc .. g LOOP

      FOR am1 in reverse 0 .. c(k)-1 LOOP -- a counts codes of length k

        -- here i is the huffman code of length k bits for value v(v_idx)
        WHILE k > w + l(h) LOOP

          w:= w + l(h);    -- Length of tables to this position
          h:= h + 1;
          z:= g - w;       -- Compute min size table <= m bits
          IF z > m THEN z := m; END IF;
          j := k - w;
          f := integer(shift_left(unsigned_32'(1), j)); -- f:= 2 ** j;
          IF f > am1 + 2 THEN   -- Try a k-w bit table
            f:= f - (am1 + 2);
            c_idx:= k;
            loop              -- Try smaller tables up to z bits
              j:= j + 1;
              exit when j >= z;
              f := f * 2;
              c_idx:= c_idx + 1;
              exit when f <= c(c_idx);
              f:= f - c(c_idx);
            end loop;
          END IF;

          IF   w + j > el and then  w < el  THEN
            j:= el - w;       -- Make EOB code end at table
          END IF;
          IF w = 0 THEN
            j := m;  -- *** Fix: main table always m bits!
          END IF;
          z    := integer(shift_left(unsigned_32'(1), j)); -- z:= 2 ** j;
          l(h) := j;

          -- Allocate and link new table

          begin
            current_table_ptr := New HufT_table ( 0..z );
            new_node_ptr      := New Table_list'( current_table_ptr, null );
          exception
            when storage_error => raise huft_out_of_memory;
          end;

          if first_table then
            tl          := new_node_ptr;
            last_list   := new_node_ptr;
            first_table := false;
          else
            current_node_ptr.next:= new_node_ptr;   -- not my first...
          end if;

          current_node_ptr:= new_node_ptr;

          u( h ):= current_table_ptr;

          -- Connect to last table, if there is one

          IF h > 0 THEN
            x(h) := i;
            new_entry.b   := byte(l(h-1));
            new_entry.e   := byte(16 + j);
            new_entry.nxt := current_table_ptr;

            j :=  integer(
             shift_right( unsigned_32(i) AND
               (shift_left(unsigned_32'(1), w) - 1 ), w - l(h-1) )
                   );

            -- Test against bad input!

            IF j > u( h - 1 )'last then
              raise huft_error;
            END IF;
            u( h - 1 ) (j) := new_entry;
          END IF;

        END LOOP;

        -- Set up table entry in new_entry

        new_entry.b   := Byte( k - w );
        new_entry.nxt := Null;   -- Unused

        IF v_idx >= n THEN
          new_entry.e := 99;
        else
          el_v:= v(v_idx);
          el_v_m_s:= el_v - s;
          IF el_v_m_s < 0 THEN -- Simple code, raw value
            IF el_v < 256 THEN
              new_entry.e:= 16;
            ELSE
              new_entry.e:= 15;
            END IF;
            new_entry.n := el_v;
          ELSE                    -- Non-simple -> lookup in lists
            IF no_copy_length_array THEN
              raise huft_error;
            END IF;
            new_entry.e := byte(    e( el_v_m_s ) );
            new_entry.n := integer( d( el_v_m_s ) );
          END IF;
          v_idx:= v_idx + 1;
        END IF;

        -- fill code-like entries with new_entry
        f := integer( shift_left( unsigned_32'(1) , ( k - w ) ));
        -- i.e. f := 2 ** (k-w);
        j := integer( shift_right( unsigned_32(i), w ) );
        WHILE j < z LOOP
          current_table_ptr(j) := new_entry;
          j:= j + f;
        END LOOP;

        -- backwards increment the k-bit code i
        j := integer( shift_left( unsigned_32'(1) , ( k - 1 ) ));
        -- i.e.: j:= 2 ** (k-1)
        WHILE ( unsigned_32(i) AND unsigned_32(j) ) /= 0 LOOP
          i := integer( unsigned_32(i) XOR unsigned_32(j) );
          j :=  j / 2;
        END LOOP;
        i := integer( unsigned_32(i) XOR unsigned_32(j) );

        -- backup over finished tables
        WHILE integer(unsigned_32(i) and (shift_left(1, w)-1)) /= x(h) LOOP
          h:= h - 1;
          w:= w - l(h); -- Size of previous table!
        END LOOP;

      END LOOP;  -- am1
    END LOOP;  -- k

    if trace then Text_IO.Put_Line("finished]"); end if;

    IF y /= 0 and then g /= 1 THEN raise huft_incomplete; end if;

  exception
    when huft_incomplete => raise;         -- Don't destroy the Huffmann tree
    when others => HufT_free( tl ); raise;
  end huft_build;

end UnZ_HufT;
