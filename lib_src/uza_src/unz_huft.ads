-- UnZip-Ada
------------
-- Huffman tree generating and destroying

-- Data structure rewritten by G. de Montmollin

with Interfaces;                        use Interfaces;
with UnZ_Glob;                          use UnZ_Glob;
with UnZ_Tabl;                          use UnZ_Tabl;

package UnZ_HufT is

  -- Possible exceptions occuring in huft_build

  huft_incomplete,   -- Incomplete tree <- sufficient in some cases!
  huft_error,                    -- bad tree constructed
  huft_out_of_memory: exception; -- not enough memory

  type HufT_table;
  type p_HufT_table is access HufT_table;

  type HufT is RECORD
       e   : byte;          -- # of extra bits
       b   : byte;          -- # of bits in code
       n   : integer;       --        (has been v_n: ush=unsigned_16)
       nxt : p_HufT_table;  -- next Huffman array
  END record;

  type p_HufT is access all HufT;

  type HufT_table is array( integer range <> ) of aliased huft;

  -- Linked list just for destroying Huffman tables

  type Table_list;
  type p_Table_list is access Table_list;

  type Table_list is record
    table: p_HufT_table;
    next : p_Table_list;
  end record;

  last_list: p_Table_list; -- to get built tree after huft_incomplete

  -- *************** free huffman tables starting with table where t points to
  procedure HufT_free ( tl: in out p_Table_list );

  -- *********** build huffman table from code lengths given by array b.all

  procedure HufT_Build ( b    : unsigned_16_array;
                         s    : integer;
                         d, e : copy_length_array;
                         tl   : in out p_Table_list;
                         m    : in out integer);

end UnZ_HufT;
