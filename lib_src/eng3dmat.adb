-----------------------------------------------------------------------------
--  File: eng3dmat.adb; see specification (eng3dmat.ads)
-----------------------------------------------------------------------------

with Screen.Effects;

package body Engine_3D.Math is

  use Screen.Effects.El_Func; -- G.E.F. for sqrt,sin,cos,etc. on type 'Real'

  -- The fixed-point is home-made, so are the floor and ceiling functions
  function Floor(f:fix) return integer is
    begin return f / fix_one; end;

  function Ceiling(f:fix) return integer is
    begin
      if f mod fix_one = 0 then
        return f / fix_one;
      else
        return 1 + f / fix_one;
      end if;
    end;

  -- Vectors & matrices

  function To_RealPoint(p: Point) return RealPoint is
    begin return (Real(p.x),Real(p.y),Real(p.z)); end;
  
  function To_RealPoint(p: Fixed_Point) return RealPoint is
    begin return (Real(p.x/fix_one),Real(p.y/fix_one),Real(p.z/fix_one)); end;

  function To_point(p: RealPoint) return Point is
    begin return (integer(p(1)),integer(p(2)),integer(p(3))); end;

  function To_fixed_point(p: RealPoint) return Fixed_Point is
  begin
    return ( x=> Fix( fl_fix_one * p(1) ),
             y=> Fix( fl_fix_one * p(2) ),
             z=> Fix( fl_fix_one * p(3) ) );
  end;
  
  function "*"(l:Real; v:vector3) return vector3 is
    r: vector3;
    begin
      for i in 1..3 loop r(i):= v(i)*l; end loop;
      return r;
    end;
  
  function "+"(a,b:vector3) return vector3 is
    r: vector3;
    begin
      for i in 1..3 loop r(i):= a(i)+b(i); end loop;
      return r;
    end;
  
  function "-"(a,b:vector3) return vector3 is
    r: vector3;
    begin
      for i in 1..3 loop r(i):= a(i)-b(i); end loop;
      return r;
    end;
  
  function "-"(a:vector3) return vector3 is
    r: vector3;
    begin
      for i in 1..3 loop r(i):= -a(i); end loop;
      return r;
    end;
  
  function "*"(a,b:vector3) return Real is    -- dot product
    begin
      return a(1)*b(1)+a(2)*b(2)+a(3)*b(3);
    end;
  
  function "*"(a,b:vector3) return vector3 is -- cross product
    begin
     return ( a(2)*b(3) - a(3)*b(2),
              a(3)*b(1) - a(1)*b(3),
              a(1)*b(2) - a(2)-b(1) );
    end;
  
  function Norm(a: Vector3) return Real is
    begin return Sqrt(a(1)*a(1)+a(2)*a(2)+a(3)*a(3)); end;
  
  function Norm2(a: Vector3) return Real is
    begin return a(1)*a(1)+a(2)*a(2)+a(3)*a(3); end;
  
  function "*"(A,B: FixedMatrix33) return FixedMatrix33 is
    r: Fix; AB: FixedMatrix33;
    begin
      for i in 1..3 loop
        for j in 1..3 loop
          r:= 0;
          for k in 1..3 loop
            r:= r + (A(i,k) * B(k,j)) / fix_one; -- because 1*1 = 1 ;-)
          end loop;
          AB(i,j):= r;
        end loop;
      end loop;
      return AB;
    end "*";
  
  function "*"(A,B: Matrix33) return Matrix33 is
    r: Real; AB: Matrix33;
    begin
      for i in 1..3 loop
        for j in 1..3 loop
          r:= 0.0;
          for k in 1..3 loop
            r:= r + (A(i,k) * B(k,j));
          end loop;
          AB(i,j):= r;
        end loop;
      end loop;
      return AB;
    end "*";
  
  function Transpose(A:Matrix33) return Matrix33 is
    begin
      return ( (a(1,1),a(2,1),a(3,1)),
               (a(1,2),a(2,2),a(3,2)),
               (a(1,3),a(2,3),a(3,3)));
    end;
  
  function "*"(A:matrix33; x:vector3) return vector3 is
    r: Real;
    Ax: vector3;
    begin
      for i in 1..3 loop
        r:= 0.0;
        for j in 1..3 loop
            r:= r + A(i,j) * x(j);
        end loop;
        Ax(i):= r;
      end loop;
      return Ax;
    end "*";
  
  -- Following procedure is from Project Spandex, http://www.grafix3d.tzo.com/
  procedure Orthonormalize(M: in out Matrix33) is
   dot1,dot2,vlen: Real; 
   begin
     dot1:= m(1,1) * m(2,1) + m(1,2) * m(2,2) + m(1,3) * m(2,3);
     dot2:= m(1,1) * m(3,1) + m(1,2) * m(3,2) + m(1,3) * m(3,3);
                                                                      
     m(1,1) := m(1,1) - dot1 * m(2,1) - dot2 * m(3,1);
     m(1,2) := m(1,2) - dot1 * m(2,2) - dot2 * m(3,2);
     m(1,3) := m(1,3) - dot1 * m(2,3) - dot2 * m(3,3);
                                                                      
     vlen:= 1.0 / sqrt(m(1,1) * m(1,1) +                            
                       m(1,2) * m(1,2) +                            
                       m(1,3) * m(1,3));                            
                                                                      
     m(1,1):= m(1,1) * vlen;
     m(1,2):= m(1,2) * vlen;
     m(1,3):= m(1,3) * vlen;
                                                                      
     dot1:= m(2,1) * m(1,1) + m(2,2) * m(1,2) + m(2,3) * m(1,3);
     dot2:= m(2,1) * m(3,1) + m(2,2) * m(3,2) + m(2,3) * m(3,3);
  
     m(2,1) := m(2,1) - dot1 * m(1,1) - dot2 * m(3,1);
     m(2,2) := m(2,2) - dot1 * m(1,2) - dot2 * m(3,2);
     m(2,3) := m(2,3) - dot1 * m(1,3) - dot2 * m(3,3);
  
     vlen:= 1.0 / sqrt(m(2,1) * m(2,1) +                            
                       m(2,2) * m(2,2) +                            
                       m(2,3) * m(2,3));                            
  
     m(2,1):= m(2,1) * vlen;                                                 
     m(2,2):= m(2,2) * vlen;                                                 
     m(2,3):= m(2,3) * vlen;                                                 
  
     m(3,1):= m(1,2) * m(2,3) - m(1,3) * m(2,2);
     m(3,2):= m(1,3) * m(2,1) - m(1,1) * m(2,3);
     m(3,3):= m(1,1) * m(2,2) - m(1,2) * m(2,1);
   end Orthonormalize;
  
   function To_Real(FM: FixedMatrix33) return Matrix33 is
    RM: Matrix33;
    begin
      for i in FM'range(1) loop
        for j in FM'range(2) loop
          RM(i,j):= Real(FM(i,j)) * iv_fl_fix_one;
        end loop;
      end loop;
      return RM;
    end;
  
   function To_Fix(RM: Matrix33) return FixedMatrix33 is
    FM: FixedMatrix33;
    begin
      for i in RM'range(1) loop
        for j in RM'range(2) loop
          FM(i,j):= Fix( fl_fix_one * RM(i,j) );
        end loop;
      end loop;
      return FM;
    end;
  
  function XYZ_rotation(ax,ay,az: Real) return FixedMatrix33 is
    Mx, My, Mz: FixedMatrix33; c,s: Fix;
    begin
      -- Around X
      c:= fix( fl_fix_one * cos( ax ) );
      s:= fix( fl_fix_one * sin( ax ) );
      Mx:= ( (fix_one,0,0),  (0, c , -s ),   (0, s, c ) );
      -- Around Y
      c:= fix( fl_fix_one * cos( ay ) );
      s:= fix( fl_fix_one * sin( ay ) );
      My:= ( ( c ,0, -s ),  (0,fix_one,0),   ( s,0, c ) );
      -- Around Z
      c:= fix( fl_fix_one * cos( az ) );
      s:= fix( fl_fix_one * sin( az ) );
      Mz:= ( ( c , -s ,0),  ( s, c ,0),   (0,0,fix_one) );
  
      return Mz * My * Mx;
    end XYZ_rotation;
  
  function XYZ_rotation(ax,ay,az: Real) return Matrix33 is
    Mx, My, Mz: Matrix33; c,s: Real;
    begin
      -- Around X
      c:= cos( ax );
      s:= sin( ax );
      Mx:= ( (1.0,0.0,0.0),  (0.0, c , -s ),   (0.0, s, c ) );
      -- Around Y
      c:= cos( ay );
      s:= sin( ay );
      My:= ( ( c ,0.0, -s ),  (0.0,1.0,0.0),   ( s,0.0, c ) );
      -- Around Z
      c:= cos( az );
      s:= sin( az );
      Mz:= ( ( c , -s ,0.0),  ( s, c ,0.0),   (0.0,0.0,1.0) );
  
      return Mz * My * Mx;
    end XYZ_rotation;
 
 
 end Engine_3D.Math;
