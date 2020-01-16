------------------------------------------------------------------------------
--  File:            Eng3DPor.ads
--  Description:     Basic 3D engine - functions for portal rendering
--  Date / Version:  6-Aug-2000
--  Author's e-mail: gdemont@hotmail.com
--
--  Copyright (c) Gautier de Montmollin 2000
--  CH-2000 Neuchatel
--  SWITZERLAND
--  Permission granted to use this software, without any warranty, for any
--  purpose, provided this copyright note remains attached and unmodified.
------------------------------------------------------------------------------

package Engine_3D.Portals is

  -- Cheap but fast method with rectangles.
  -- Two inconveniants of using only this:
  --  1. Normally, the bounding rectangle is larger than a face
  --  2. Even with sorting, you see this problem
  --  3. With an exact portal clipping, you can render connected
  --     convex polygons without sorting at all :-)

  procedure Intersect (A,B: t_rect; C: out t_rect; non_empty: out Boolean);
  procedure Find_bounding_box(o: Object_3D; face: Natural; b: out t_rect);

end Engine_3D.Portals;
