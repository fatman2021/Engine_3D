------------------------------------------------------------------------------
--  File:            Engine3D.ads
--  Description:     Basic 3D engine
--  Main reference:  Peroxide tutorial #4, http://www.peroxide.dk/
--
--  Date / Version:  15-Oct-2000
--  Author's e-mail: gdemont@hotmail.com
--
--  Copyright (c) Gautier de Montmollin 1999,2000
--  CH-2000 Neuchatel
--  SWITZERLAND
--  Permission granted to use this software, without any warranty, for any
--  purpose, provided this copyright note remains attached and unmodified.
------------------------------------------------------------------------------

with Graphics.Colors;                   use Graphics;
with Screen;                            use Screen;
with Screen.Effects;                    use Screen.Effects;
with Ada.Unchecked_deallocation;

package Engine_3D is

  subtype Real is Screen.Effects.Real;

  -- "hand-made" fixed point type for coordinates
  subtype Fix is integer;     -- no, it isn't an Ada fixed point type
  fix_bits: constant:= 9;
  fix_one:  constant:= 2**fix_bits;   -- 1.0 <-> fix_one
  fl_fix_one   : constant Real:= Real(fix_one);
  iv_fl_fix_one: constant Real:= 1.0 / fl_fix_one;

  type Fix_array is Array(natural range <>) of fix;

  type Point is       record x,y,z: integer; end record;
  type Fixed_point is record x,y,z: fix;     end record;

  type ScrPoint is       record x,y: integer; end record;
  type Fixed_ScrPoint is record x,y: fix;     end record;

  ------------------------
  -- Vectors & matrices --
  ------------------------
  type Vector3 is array(1..3) of Real;
  subtype RealPoint is Vector3;
  subtype Fixed_vectT is Fixed_point;
  -- for purely moral reasons I prefer to distinguish points and vectors...

  -- Operators & functions in Engine_3D.Math package (see Eng3DMat.ads)

  type Matrix33 is array(1..3,1..3) of Real;
  Id33: constant Matrix33:= ( (1.0,0.0,0.0), (0.0,1.0,0.0), (0.0,0.0,1.0) );

  type FixedMatrix33 is array(1..3,1..3) of Fix;
  FixedId33: constant FixedMatrix33:=
    ( (fix_one,0,0), (0,fix_one,0), (0,0,fix_one) );

  -------------------
  -- Define a face --
  -------------------

  type Index_array is array(natural range <>) of Natural;

  type Face is record
     P: Index_array(1..4);  -- indices of the edges (anticlockw.)
                  -- one of them can be 0 (triangle); then the
                  -- "missing" edge indicates how to put texture

     textured: Boolean;            -- for auto-choice in Draw procedure

     -- *** Texture-mapping part:

     texture:       p_Texture_map:= null;
     rep_U, rep_V:  positive;
     intens_min,
     intens_max:    intensity;  -- min: darkest, max: brightest

     -- *** Colour-only part:

     color_min,
     color_max: Colors.Color_Index; -- color_min: darkest, color_max: brightest
  end record;

  subtype Ident is String(1..16); -- for naming things
  type Point_array is Array(natural range <>) of Point;
  type Fixed_point_array is Array(natural range <>) of Fixed_point;
  type Fixed_vect_array is Array(natural range <>) of Fixed_vectT;
  type ScrPoint_array is Array(natural range <>) of ScrPoint;
  type Fixed_ScrPoint_array is Array(natural range <>) of Fixed_ScrPoint;
  type Face_array is Array(natural range <>) of Face;
  type Center_array is Array(natural range <>) of integer;
  type Bool_array is Array(natural range <>) of boolean;

  Max_faces_per_vertex: constant:= 32;
  type adjacence_table is array(natural range <>, natural range <>) of natural;

  type t_rect is record X1,Y1,X2,Y2: Integer; end record;
  type t_rect_array is Array(natural range <>) of t_rect;

  type Object_3D;
  type p_Object_3D is access Object_3D;

  -- Portals
  type t_Portal_type is ( just_a_face, see_through, mirror );

  type t_Portal is record
    Portal_type : t_Portal_type:= just_a_face;
    Invisible   : Boolean:= False; -- False: texture shown (glass, grids,... )
    Connecting  : p_Object_3D:= Null;
  end record;

  -- NB: the default values match the "normal", single object model.

  type t_Portal_array is array(natural range <>) of t_Portal;

  -----------------------------------
  -- Now: the Object_3D definition --
  -----------------------------------

  -- 15-Oct-2000: fields Points & Projected are fixed-point for subpixel accuracy

  type Object_3D(Max_points, Max_faces: integer) is record

    -- The following are data:

    Baseobj    : Point_array(1..Max_points);  -- unmoved, relative edges
    Faces      : Face_array(1..Max_faces);    -- faces

    Num_of_points : integer:= Max_points;
    Num_of_faces  : integer:= Max_faces;

    Center        : RealPoint:= (0.0,0.0,0.0);  -- absolute centering
    Auto_rotation : FixedMatrix33:= FixedId33;  -- object orientation
    -- these default values are important for merging!

    -- The following will be calculated

    Points     : Fixed_Point_array(1..Max_points); -- rotated & moved 3D pts
    Projected  : Fixed_ScrPoint_array(1..Max_points);  -- the 2D-screenpoints
    Center_Z   : Center_array(1..Max_faces);     -- Z-val of centres, SORTED
    Order      : Index_array(1..Max_faces);       -- order after sorting
    Normals,                                      -- outer normal vectors
    RotNormals : Fixed_vect_array(1..Max_faces); -- rotated normal vectors

    num_of_adjacents: Index_array(1..Max_points);
    adjacent   : adjacence_table(1..Max_points, 1..Max_faces_per_vertex);

    visible      : bool_array(1..Max_points);
    visible_face : bool_array(1..Max_faces);

    -- Again data (are here for cache efficiency), defaulted

    Portal : t_Portal_array(1..Max_faces);   -- An object can be a room

    Id : Ident:= "Nameless        ";         -- Name of the object
    projection_refreshed : boolean;
    sorting_refreshed    : boolean;   -- sometimes no sorting needed
  end record; -- Object_3D

  -- Object initialisation (once in object's life)
  procedure Init_object(o: in out Object_3D);

  -- arrays of pointers to objects
  type a_p_Object_3D is array(integer range <>) of p_Object_3D;

  -------------------------
  -- Projection 3D -> 2D --
  -------------------------

  procedure Project(p3d    :  in Fixed_Point;
                    p2d    : out Fixed_ScrPoint;
                    visible: out boolean);

  procedure Project(p3d: in Point; p2d: out ScrPoint; visible: out boolean);

  -- Projects whole object and calculates center Z-val for sorting

  procedure Project(o: in out Object_3D);

  -------------
  -- Sorting --
  -------------
  procedure Sort_faces(o: in out object_3D);            -- Z sorting

  ----------------------------
  -- The Object_3D drawing  --
  ----------------------------
  type shading_mode is ( Z_only, Lambert, Gouraud, Phong );

  type surface_select is ( colors_only, textures_only, auto );
    -- auto means: trust the Face.textured (true/false) item.
    
  type texture_mapping_mode is (
    affine_y_affine_x,  -- everything affine            -> N.z ~= 1 (facing)
    npersp_y_affine_x,  -- ends: persp, H-lines: affine -> N.x ~= 0 (floors)
    affine_y_npersp_x,  -- ends: persp, V-lines: affine -> N.y ~= 0 (walls)
    npersp_y_npersp_x,  -- everything near-perspective (every n pixels)
    auto );             -- auto select according to normal N

--  Procedure Draw (buffer        : out Screen_Buffer;
--                  o             : in out Object_3D;
--                  surf_select   : in  surface_select;
--                  map_mode      : in  texture_mapping_mode;
--                  shading       : in  shading_mode;
--                  do_Z_shading  : in  boolean:= false;
--                  minZ, maxZ    : in  integer:= 0
--                 );
                 
  Procedure Draw (o             : in out Object_3D;
                  surf_select   : in  surface_select;
                  map_mode      : in  texture_mapping_mode;
                  shading       : in  shading_mode;
                  do_Z_shading  : in  boolean:= false;
                  minZ, maxZ    : in  integer:= 0
                 );
                 
  -- N.B.: Z-shading is added to others

  ------------------------------------------------------------------------
  -- Autonomous_Object_3D definition: that object has its own cinematic --
  ------------------------------------------------------------------------

--   type Autonomous_Object_3D is new Object_3D with record
--     Speed, Acceleration: Vector3;
--   end record;

  ---------------
  -- Lightings --
  ---------------

  -- Parallel lights (sun, distant sources,... )
  max_parallel_lights: constant:= 4;
  parallel_lights: natural:= 0;
  parallel_light_vect:  array(1..max_parallel_lights) of Vector3;
  parallel_light_force: array(1..max_parallel_lights) of Real;

  -- Radial lights (bulb,... )
  max_radial_lights: constant:= 10;
  radial_lights: natural:= 0;
  radial_light_source: array(1..max_radial_lights) of RealPoint;
  radial_light_force:  array(1..max_radial_lights) of Real;

-- NB: the lights must be rotated (Rotate_lights) !

  Procedure Find_light_direction( Pbeg, Pend: Point; dir: out Vector3 );

  -----------------------------
  -- Set engine's parametres --
  -----------------------------

     screen_virtual_size: constant:= 1024;
  fl_screen_virtual_size: constant:= 1024.0;

  -- adimensioned screen size, to lift dependency to resolution
  -- screen_virtual_size represents the height of the screen

  procedure Init_engine(
    X_res, Y_res: Positive;  -- resolution e.g. 640x400

    X_clip_left, 
    X_clip_right,
    Y_clip_top,
    Y_clip_bottom: Natural;  -- screen clipping bounds e.g. 0,639,0,399
    Z_clip_min: Real;        -- Z clipping is adimensioned,
                             -- e.g. fl_screen_virtual_size / 10.0
    X_offset,
    Y_offset: Natural;       -- (0,0,0) is projected to (X_offset,Y_offset)
    Focal: Real              -- adimensioned focal length
                             -- e.g. fl_screen_virtual_size
   );

-- NB: the eye is now at (0,0,0) (was (0,0,-Focal))

  ----------------
  -- The Camera --
  ----------------

  Eye: Vector3:= (others=> 0.0);   -- position of the eye in the universe...

  World_rotation: Matrix33:= Id33; -- inverse of Camera rotation...

  procedure Rotate_lights;         -- the lightings must follow ! (1x per image)

-- Free allocated memory

  procedure Dispose is new Ada.Unchecked_Deallocation(Object_3D,p_Object_3D);

-- Profiling: mode for adjusting some constants
  scanline_profiling: boolean:= true;

end Engine_3D;
