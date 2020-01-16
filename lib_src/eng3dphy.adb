-----------------------------------------------------------------------------
--  File: eng3dphy.adb; see specification (eng3dphy.ads)
-----------------------------------------------------------------------------

-- with Engine_3D.Sorting;
-- with SVGA;                              use SVGA;
-- with SVGA.Effects;                      use SVGA.Effects;
-- with Interfaces;                        use Interfaces;
-- with Ada.Numerics;                      use Ada.Numerics;
-- with Ada.Exceptions;                    use Ada.Exceptions;
 
-- with Min_Max;

package body Engine_3D.Physics is

  function Inside_convex( p: Vector3; o: Object_3D ) return Boolean is
    dot: Real; s: Natural; p_to_vertex: Vector3;
    begin
      for face in reverse 1..o.Num_of_faces loop
        for sf in o.Faces(face).P'range loop
          s:= o.Faces(face).P(sf);
          exit when s/=0;
        end loop;
        p_to_vertex:= p - To_vector3(o.Points(s));
        dot:= To_vector3(o.RotNormals(face)) * p_to_vertex;
        if dot < 0.0 then return False; end if;
      end loop;
      return True;
    end Inside_convex;



end Engine_3D.Physics;
