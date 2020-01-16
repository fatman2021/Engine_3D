package body Engine_3D.Portals is

  procedure Intersect (A,B: t_rect; C: out t_rect; non_empty: out Boolean) is
    begin
      C.X1:= Integer'Max(A.X1,B.X1);
      C.X2:= Integer'Min(A.X2,B.X2);
      C.Y1:= Integer'Max(A.Y1,B.Y1);
      C.Y2:= Integer'Min(A.Y2,B.Y2);
      non_empty:= C.X1 <= C.X2 and C.Y1 <= C.Y2;
    end Intersect;

  procedure Find_bounding_box(o: Object_3D; face: Natural; b: out t_rect) is
    s: Natural; sp: Fixed_ScrPoint;
    begin
      b:= ( X1|Y1=> Integer'Last, X2|Y2=> Integer'First );

      for sf in o.Faces(face).P'range loop
        s:= o.Faces(face).P(sf);
        if s/=0 then
          sp:= o.Projected(s);
          b.X1:= Integer'Min(b.X1, sp.x/fix_one);
          b.X2:= Integer'Max(b.X2, sp.x/fix_one);
          b.Y1:= Integer'Min(b.Y1, sp.y/fix_one);
          b.Y2:= Integer'Max(b.Y2, sp.y/fix_one);
        end if;
      end loop;
    end Find_bounding_box;

end Engine_3D.Portals;
