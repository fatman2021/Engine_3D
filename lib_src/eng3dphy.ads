------------------------------------------------------------------------------
--  File:            eng3dphy.ads
--  Description:     Basic 3D engine - Geometry, cinematic, mechanics
--  Date / Version:  14-Dec-1999
--  Author's e-mail: Gautier.deMontmollin@Maths.UniNe.CH
--
--  Copyright (c) Gautier de Montmollin 1999
--  CH-2000 Neuchatel
--  SWITZERLAND
--  Permission granted to use this software, without any warranty, for any
--  purpose, provided this copyright note remains attached and unmodified.
------------------------------------------------------------------------------

package Engine_3D.Physics is

  function Inside_convex( p: Vector3; o: Object_3D ) return Boolean;

end Engine_3D.Physics;
