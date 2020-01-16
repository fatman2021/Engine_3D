------------------------------------------------------------------------------
--  File:            min_max.ads
--  Description:     Generic Min and Max functions
--  Date/version:    20.IX.1997
--  Author:          Gautier.deMontmollin@Maths.UniNe.CH
------------------------------------------------------------------------------
generic
     type comparable is private;
     with function ">" (left,right:comparable) return boolean is <>;

package Min_Max is

 -- Min / Max for 2 elements: 
 -- n.b.: for a scalar type S, Ada 95 has attributes S'Min(.,.), S'Max(.,.)
     function Min(a,b:comparable) return comparable;
     function Max(a,b:comparable) return comparable;
     
 -- Min / Max for any number of elements:
     type multiple_comparable_data is array(integer range <>) of comparable;
     function Min(m:multiple_comparable_data) return comparable;
     function Max(m:multiple_comparable_data) return comparable;
end;
