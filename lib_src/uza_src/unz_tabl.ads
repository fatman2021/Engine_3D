-- UnZip-Ada
------------
-- Tables for bit masking, huffman codes and CRC checking

-- Note from Pascal source:
-- C code by info-zip group, translated to pascal by Christian Ghisler
-- based on unz51g.zip

with Interfaces;                        use Interfaces;

package UnZ_Tabl is

-- b and mask_bits(i) gets lower i bits out of i

 mask_bits : constant ARRAY ( 0..16 ) OF unsigned_32 :=
  ( 16#0000#, 16#0001#, 16#0003#, 16#0007#, 16#000f#,
    16#001f#, 16#003f#, 16#007f#, 16#00ff#, 16#01ff#,
    16#03ff#, 16#07ff#, 16#0fff#, 16#1fff#, 16#3fff#,
    16#7fff#, 16#ffff# );


-- Tables for deflate from PKZIP's appnote.txt.

 -- Order of the bit length code lengths

 bit_order : constant ARRAY ( 0..18 ) OF natural :=

      ( 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 );

 type copy_length_array is array( natural range <> ) of unsigned_16;

 cp_empty : constant copy_length_array( 1..0 ) := ( others=> 0 );

 -- Copy lengths for literal codes 257..285

 cplens : constant copy_length_array( 0..30 ) :=
      (  3,  4,  5,  6,  7,  8,  9, 10, 11, 13, 15, 17, 19, 23, 27, 31,
        35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258, 0, 0 );
        -- note: see note #13 above about the 258 in this list.

 -- Extra bits for literal codes 257..285

 cplext : constant copy_length_array( 0..30 ) :=
        ( 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
          3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0, 99, 99 );    -- 99==invalid

 -- Copy offsets for distance codes 0..29

 cpdist : constant copy_length_array( 0..29 ) :=
      ( 1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193,
        257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145,
        8193, 12289, 16385, 24577 );

 -- Extra bits for distance codes

 cpdext : constant copy_length_array( 0..29 ) :=
      ( 0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
        7, 7, 8, 8, 9, 9, 10, 10, 11, 11,
        12, 12, 13, 13 );

-- Tables for explode

 cplen2 : constant copy_length_array( 0..63 ) :=
      (  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17,
        18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34,
        35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51,
        52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65 );

 cplen3 : constant copy_length_array( 0..63 ) :=
      (  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
        19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35,
        36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52,
        53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66 );

 extra : constant copy_length_array( 0..63 ) :=
      ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        8 );

 cpdist4 : constant copy_length_array( 0..63 ) :=
      ( 1, 65, 129, 193, 257, 321, 385, 449, 513, 577, 641, 705,
        769, 833, 897, 961, 1025, 1089, 1153, 1217, 1281, 1345, 1409, 1473,
        1537, 1601, 1665, 1729, 1793, 1857, 1921, 1985, 2049, 2113, 2177,
        2241, 2305, 2369, 2433, 2497, 2561, 2625, 2689, 2753, 2817, 2881,
        2945, 3009, 3073, 3137, 3201, 3265, 3329, 3393, 3457, 3521, 3585,
        3649, 3713, 3777, 3841, 3905, 3969, 4033 );

 cpdist8 : constant copy_length_array( 0..63 ) :=
      (    1,  129,  257,  385,  513,  641,  769,  897, 1025, 1153, 1281,
        1409, 1537, 1665, 1793, 1921, 2049, 2177, 2305, 2433, 2561, 2689,
        2817, 2945, 3073, 3201, 3329, 3457, 3585, 3713, 3841, 3969, 4097,
        4225, 4353, 4481, 4609, 4737, 4865, 4993, 5121, 5249, 5377, 5505,
        5633, 5761, 5889, 6017, 6145, 6273, 6401, 6529, 6657, 6785, 6913,
        7041, 7169, 7297, 7425, 7553, 7681, 7809, 7937, 8065 );

end UnZ_Tabl;
