package body Engine_3D.Construct is

  total_points_exhausted: exception;
  total_faces_exhausted : exception;

  procedure Find_or_create( p: Point; o: in out Object_3D; idx: out Natural) is
    begin
      for i in 1 .. o.Num_of_points loop
        if o.BaseObj(i) = p then idx:= i; return; end if;
      end loop;
      o.Num_of_points:= o.Num_of_points + 1;
      if o.Num_of_points > o.Max_points then
        raise total_points_exhausted;
      end if;
      o.BaseObj(o.Num_of_points):= p;
      idx:= o.Num_of_points;
    end Find_or_create;

  procedure Create_face( f: Face;
                         p: Point_array;  -- imposed coordinates of edges
                         e: bool_array;   -- edge or not
                         o: in out Object_3D) is
    idx: array(p'range) of Natural;
    nf: Face:= f;
    begin
      for i in idx'range loop
        if e( i - idx'first + e'first ) then
          Find_or_create( p(i), o, idx(i) );
        else
          idx(i):= 0;
        end if;
      end loop;

      o.Num_of_faces:= o.Num_of_faces + 1;
      if o.Num_of_faces > o.Max_faces then
        raise total_faces_exhausted;
      end if;
      for i in idx'range loop
        nf.P( i - idx'first + 1 ):= idx(i);
      end loop;
      o.Faces(o.Num_of_faces):= nf;
    end Create_face;

end Engine_3D.Construct;
