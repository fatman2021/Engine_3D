------------------------------------------------------------------------------
--  File:            Eng3Dmer.ads
--  Description:     Basic 3D engine - Construct
--  Date / Version:  15-Oct-2000 / 18-Dec-1999
--  Author:          gdemont@hotmail.com
------------------------------------------------------------------------------

package Engine_3D.Construct is

  ---------------------------------------------------------------------------
  -- Find a point corresponding to the (local) coordinates, or add new one --
  ---------------------------------------------------------------------------

  procedure Find_or_create( p: Point; o: in out Object_3D; idx: out Natural);

  ---------------------------------
  -- Create a new face in object --
  ---------------------------------

  procedure Create_face( f: Face;
                         p: Point_array;  -- imposed coordinates of edges
                         e: bool_array;    -- edge or not
                         o: in out Object_3D);

end Engine_3D.Construct;
