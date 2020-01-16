-----------------------------------------------------------------------------
--  File: min_max.adb; see specification (min_max.ads)
-----------------------------------------------------------------------------

package body Min_Max is

  function Min(a,b:comparable) return comparable is
    begin if a>b then return b; else return a; end if; end;

  function Max(a,b:comparable) return comparable is
    begin if a>b then return a; else return b; end if; end;

  function Min(m:multiple_comparable_data) return comparable is
    a: comparable:= m(m'first); -- (constraint_error if m empty)
    begin
      for i in m'first+1 .. m'last loop
        if a>m(i) then a:= m(i); end if;
      end loop;
      return a;
    end;

  function Max(m:multiple_comparable_data) return comparable is
    a: comparable:= m(m'first); -- (constraint_error if m empty)
    begin
      for i in m'first+1 .. m'last loop
        if m(i)>a then a:= m(i); end if;
      end loop;
      return a;
    end;

  pragma Inline(Max,Min);

end;
