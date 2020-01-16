------------------------------------------------------------------------------
--  File:            Eng3DMat.ads
--  Description:     Basic 3D engine - math, matrices, operators
--  Date / Version:  15-Oct-2000 / 6-Aug-2000
--  Author's e-mail: gdemont@hotmail.com
--
--  Copyright (c) Gautier de Montmollin 2000
--  CH-2000 Neuchatel
--  SWITZERLAND
--  Permission granted to use this software, without any warranty, for any
--  purpose, provided this copyright note remains attached and unmodified.
------------------------------------------------------------------------------

package Engine_3D.Math is

  -- The fixed-point is home-made, so are the floor and ceiling functions
  function Floor(f:fix) return integer;
  function Ceiling(f:fix) return integer;

  pragma Inline(Floor,Ceiling);

  -- Vectors & matrices

  function To_RealPoint(p: Point) return RealPoint;
  function To_RealPoint(p: Fixed_Point) return RealPoint;
  function To_point(p: RealPoint) return Point;
  function To_fixed_point(p: RealPoint) return Fixed_Point;

  function "*"(l:Real; v:Vector3) return Vector3;
  function "+"(a,b: Vector3) return Vector3;
  function "-"(a,b: Vector3) return Vector3;
  function "-"(a: Vector3) return Vector3;
  function "*"(a,b:vector3) return Real;     -- dot product
  function "*"(a,b:vector3) return vector3;  -- cross product
  function Norm(a: Vector3) return Real;
  function Norm2(a: Vector3) return Real;

  function To_Real(FM: FixedMatrix33) return Matrix33;
  function To_Fix(RM: Matrix33) return FixedMatrix33;

  function "*"(A,B: FixedMatrix33) return FixedMatrix33;
  function "*"(A,B: Matrix33) return Matrix33;
  function "*"(A:matrix33; x:vector3) return vector3;

  function Transpose(A:Matrix33) return Matrix33;
  procedure Orthonormalize(M: in out Matrix33);

  function XYZ_rotation(ax,ay,az: Real) return FixedMatrix33;
  function XYZ_rotation(ax,ay,az: Real) return Matrix33;
  -- A matrix is calculated once for each object and once for
  -- the whole view so we can avoid using sin/cos tables

end Engine_3D.Math;
