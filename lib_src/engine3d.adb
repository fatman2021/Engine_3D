-----------------------------------------------------------------------------
--  File: engine3d.adb; see specification (engine3d.ads)
-----------------------------------------------------------------------------

with Engine_3D.Math;                    use Engine_3D.Math;
with Engine_3D.Portals;                 use Engine_3D.Portals;
with Engine_3D.Sorting;

with Screen;                            use Screen;
with Screen.Effects;                    use Screen.Effects;
with Interfaces;                        use Interfaces;
with Ada.Numerics;                      use Ada.Numerics;
with Ada.Exceptions;                    use Ada.Exceptions;

with Min_Max, Scanline_profiler;

package body Engine_3D is

  -- Generic Min / Max
  package MMI is new Min_Max(integer); use MMI;

  -- Engine's variables 

  Xres, Yres, Xofs, Yofs: Integer;
  Zmin, Focal, corrected_focal: Real;
  x_overflow_left, x_overflow_right, 
  y_overflow_top,  y_overflow_bottom: Real;

  -- same, for fixed-point projection
  fx_Xofs, fx_Yofs: fix;
  fx_x_overflow_left, fx_x_overflow_right, 
  fx_y_overflow_top,  fx_y_overflow_bottom: Real;

  main_clipping_rect: t_rect;

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
   ) is

    max_fixed_one: constant integer:= max(fix_one, sfix_one);
    int_limit: constant:= 4000.0; -- (pixels)
    -- integer:= integer'last / max_fixed_one - 1;

    begin
      Xres:= X_res-1;  Yres:= Y_res-1;
      Xofs:= X_offset; Yofs:= Y_offset;
      fx_Xofs:= fix_one * Xofs;
      fx_Yofs:= fix_one * Yofs;
      main_clipping_rect:=
        ( X1=> X_clip_left,  Y1=> Y_clip_top,
          X2=> X_clip_right, Y2=> Y_clip_bottom );

      x_overflow_left  := - int_limit - Real(X_offset);
      x_overflow_right :=   int_limit - Real(X_offset);
      y_overflow_top   := - int_limit - Real(Y_offset);
      y_overflow_bottom:=   int_limit - Real(Y_offset);

      fx_x_overflow_left  := x_overflow_left   * fl_fix_one;
      fx_x_overflow_right := x_overflow_right  * fl_fix_one;
      fx_y_overflow_top   := y_overflow_top    * fl_fix_one;
      fx_y_overflow_bottom:= y_overflow_bottom * fl_fix_one;

      Zmin:=        Z_clip_min;
      Engine_3D.Focal:= Focal;
      corrected_focal:= Focal * Real(Y_res) / fl_screen_virtual_size;
    end;

  use Screen.Effects.El_Func;

procedure PointNormal(o: Object_3D; nr: integer; result: in out Vector3) is
-- result is the average values of the normals to the faces in which the point
-- appear; the output vector is normalized OR 0
  SumX,SumY,SumZ: fix:= 0;
  length: Real;

  begin

    for i in reverse 1 .. o.num_of_adjacents(nr) loop
      declare RN: Fixed_point renames o.RotNormals( o.adjacent(nr,i) );
      begin
        SumX := SumX + RN.X;
        SumY := SumY + RN.Y;
        SumZ := SumZ + RN.Z;
      end;
    end loop;

    result:= ( Real(SumX), Real(SumY), Real(SumZ) );
    length:= Norm( result );
    if length/=0.0 then result:= 1.0/length * result; end if;

  end PointNormal;

-- fixed point version:

procedure PointNormal(o: Object_3D; nr: integer; result: in out Fixed_point) is
  temp: Vector3;
  begin
    PointNormal(o, nr, temp);
    Result.X := fix( fl_fix_one * temp(1) );
    Result.Y := fix( fl_fix_one * temp(2));
    Result.Z := fix( fl_fix_one * temp(3) );
  end PointNormal;

  -- Internal rotation procedures:

  MR: FixedMatrix33; -- current rotation matrix

  procedure Rotate(Pin: in Point; Pout: out Point) is
  begin
    Pout.X:= (MR(1,1)*Pin.X + MR(1,2)*Pin.Y + MR(1,3)*Pin.Z) / fix_one;
    Pout.Y:= (MR(2,1)*Pin.X + MR(2,2)*Pin.Y + MR(2,3)*Pin.Z) / fix_one;
    Pout.Z:= (MR(3,1)*Pin.X + MR(3,2)*Pin.Y + MR(3,3)*Pin.Z) / fix_one;
  end Rotate;

  -- 15-Oct-2000: rotation integer -> fixed-point

  procedure Rotate(Pin: in Point; Pout: out Fixed_Point) is
  begin
    Pout.X:= (MR(1,1)*Pin.X + MR(1,2)*Pin.Y + MR(1,3)*Pin.Z);
    Pout.Y:= (MR(2,1)*Pin.X + MR(2,2)*Pin.Y + MR(2,3)*Pin.Z);
    Pout.Z:= (MR(3,1)*Pin.X + MR(3,2)*Pin.Y + MR(3,3)*Pin.Z);
  end Rotate;

  procedure Rotate(Pin: in Fixed_point; Pout: out Fixed_point) is
  begin
    Pout.X:= (MR(1,1)*Pin.X + MR(1,2)*Pin.Y + MR(1,3)*Pin.Z) / fix_one;
    Pout.Y:= (MR(2,1)*Pin.X + MR(2,2)*Pin.Y + MR(2,3)*Pin.Z) / fix_one;
    Pout.Z:= (MR(3,1)*Pin.X + MR(3,2)*Pin.Y + MR(3,3)*Pin.Z) / fix_one;
  end Rotate;

pll_lght: array(1..max_parallel_lights) of Vector3;
rdl_lght: array(1..max_radial_lights) of RealPoint;

procedure Rotate_lights is
  begin
    for i in reverse 1..parallel_lights loop
      pll_lght(i):= World_rotation * parallel_light_vect(i);
    end loop;
    for i in reverse 1..radial_lights loop
      rdl_lght(i):= World_rotation * (radial_light_source(i)-Eye);
    end loop;
  end;

procedure Init_object(o: in out Object_3D) is
  points_overflow, faces_overflow: exception;
  N: Vector3;
  laengde : Real;

  procedure Check_faces is

    procedure Check(f,v: integer) is -- f is just to have it in trace-back
      bad_vertex_number: exception;
      begin
        if v<0 or v>o.num_of_points then
          raise_exception(bad_vertex_number'identity,
               o.id & " face="&integer'image(f) & " ver="&integer'image(v));
        end if;
      end;

    begin
      for fa in reverse 1 .. o.num_of_faces loop
        for edge_num in 1..4 loop
          Check( fa, o.faces(fa).P(edge_num) );
        end loop;
      end loop; -- fa
    end;

  procedure Find_adjacences is -- for PointNormal
    AntalHits: integer;
    point_unmatched, too_many_adjacences: exception;

    begin
      for pt in reverse 1..o.num_of_points loop
        AntalHits:= 0;
        for fa in reverse 1 .. o.num_of_faces loop
          declare the_face: Face renames o.faces(fa);
          begin
           if the_face.P(1) = pt or else the_face.P(2) = pt or else
              the_face.P(3) = pt or else the_face.P(4) = pt then
             AntalHits:= AntalHits + 1;
             if AntalHits > Max_faces_per_vertex then
              raise_exception(too_many_adjacences'identity,
               o.id & " pt="&integer'image(pt));
             end if;
             o.adjacent(pt,AntalHits):= fa;
           end if; -- in which faces does the point appear
          end;
        end loop; -- fa
        if AntalHits=0 then
         raise_exception(point_unmatched'identity,
          o.id & " pt="&integer'image(pt));
        end if;
        o.num_of_adjacents(pt):= AntalHits;
      end loop; -- pt
    end;

 procedure Add_Normal_of_3p(Pn0,Pn1,Pn2: Integer) is
   P0,P1,P2: Vector3;
   begin
     if Pn0=0 or else Pn1=0 or else Pn2=0 then return; end if;
     P0:= To_RealPoint(o.baseobj(Pn0));
     P1:= To_RealPoint(o.baseobj(Pn1));
     P2:= To_RealPoint(o.baseobj(Pn2));
     N:= N + (P1-P0)*(P2-P0) ;
   end;

 begin
  -- Some checks:
  if o.Num_of_points > o.Max_points then raise points_overflow; end if;
  if o.Num_of_faces  > o.Max_faces  then raise faces_overflow;  end if;
  Check_faces;

  Find_adjacences;

  for i in reverse 1..o.Num_of_faces loop
    declare fa: Face renames o.Faces(i);
    begin
      N:= (0.0,0.0,0.0);

      Add_Normal_of_3p( fa.P(1), fa.P(2), fa.P(4));
      -- 5.IV.1999: we sum other normals for not too flat faces
      Add_Normal_of_3p( fa.P(2), fa.P(3), fa.P(1));
      Add_Normal_of_3p( fa.P(3), fa.P(4), fa.P(2));
      Add_Normal_of_3p( fa.P(4), fa.P(1), fa.P(3));
      laengde:= Norm( N );

      o.normals(i).X := fix((N(1)/laengde) * fl_fix_one);
      o.normals(i).Y := fix((N(2)/laengde) * fl_fix_one);
      o.normals(i).Z := fix((N(3)/laengde) * fl_fix_one);

    end;
   end loop;

   o.RotNormals := o.Normals;

   for counter in o.Order'range loop
     o.Order(counter):= counter;
   end loop;

  end Init_object;

  -- pre- 15-Oct-2000 version (integers), also used by demo (show light) :

  procedure Project(p3d: in Point; p2d: out ScrPoint; visible: out boolean) is
    RZ: Real:= Real(p3d.Z);
    RX, RY, Zf: Real;
  begin
    visible:= false;
    if RZ < Zmin then return; end if;
    Zf:= corrected_focal / RZ;
    RX:= Real(p3d.X) * Zf;
    if RX < x_overflow_left or else RX > x_overflow_right then
      return; -- fixed_point will overflow
    end if;
    p2d.X := Xofs + integer( RX );
    RY:= Real(p3d.Y) * Zf;
    if RY < y_overflow_top or else RY > y_overflow_bottom then
      return; -- fixed_point will overflow
    end if;
    p2d.Y := Yofs + integer( RY );
    visible:= true;
  end Project;

  pragma Inline(Project);

  -- post- 15-Oct-2000 version (integers) :

  procedure Project(p3d    :  in Fixed_Point;
                    p2d    : out Fixed_ScrPoint;
                    visible: out boolean) is
    RZ: Real:= Real(p3d.Z) * iv_fl_fix_one;
    RX, RY, Zf: Real;
  begin
    visible:= false;
    if RZ < Zmin then return; end if;
    Zf:= corrected_focal / RZ;
    RX:= Real(p3d.X) * Zf;
    if RX < fx_x_overflow_left or else RX > fx_x_overflow_right then
      return; -- fixed_point will overflow
    end if;
    p2d.X := fx_Xofs + integer( RX );
    RY:= Real(p3d.Y) * Zf;
    if RY < fx_y_overflow_top or else RY > fx_y_overflow_bottom then
      return; -- fixed_point will overflow
    end if;
    p2d.Y := fx_Yofs + integer( RY );
    visible:= true;
  end Project;

  pragma Inline(Project);

-- Internal - pure projection (after rotations etc.)
-- for Object_3D and derived types

procedure Project_only(o: in out Object_3D) is
  points        : Fixed_Point_array     renames o.points;
  projected     : Fixed_ScrPoint_array  renames o.projected;
  center_Z      : Center_array    renames o.Center_Z;
  faces         : Face_array      renames o.faces;
  order         : Index_array     renames o.order;
  visible       : bool_array      renames o.visible;
  visible_face  : bool_array      renames o.visible_face;
  begin

    for i in reverse 1 .. o.Num_of_points loop
      Project( points(i), projected(i), visible(i) );
    end loop;

    for face_num in reverse 1 .. o.Num_of_faces loop
      declare a_face: Face renames faces(face_num);
      begin
        visible_face(face_num):= true;
        for e in reverse 1..4 loop
          if a_face.P(e) /= 0 and then not visible(a_face.P(e)) then
            visible_face(face_num):= false;
            exit;
          end if;
        end loop;
      end;
      declare
        a_face: Face renames faces(order(face_num));
        -- use previous sorting: a good approximation for the new one!
        num_edges: Natural:= 0;
      begin
        Center_Z(face_num) := 0;
        for e in reverse 1..4 loop
          if a_face.P(e) /= 0 then
            num_edges:= num_edges + 1;
            Center_Z(face_num) := Center_Z(face_num) + points(a_face.P(e)).Z;
          end if;
        end loop;
        Center_Z(face_num) := (Center_Z(face_num) / num_edges) / fix_one;
      end;
    end loop; -- face
  end Project_only;

-- Exported procedures for projection, with all necessary rotations &
-- translations performed before the projection itself

-- Let's explain the vectorial operations:
-- W: world rotation
-- A: auto-rotation
-- x: basic coordinates, relative to an eventual centre
-- c: centre
-- e: eye

-- moveable object (Object_3D):
-- We need: W * ((Ax + c) - e), = WRx + W(c-e)

procedure Project(o: in out Object_3D) is
  baseobj    : Point_array  renames o.baseobj;
  points     : Fixed_Point_array  renames o.points;
  normals    : Fixed_vect_array renames o.normals;
  rotnormals : Fixed_vect_array renames o.rotnormals;
  int_center : Point:= To_Point(o.center);
  Wce        : Fixed_Point;
  begin
    Wce:= To_fixed_point( World_Rotation * ( o.Center - Eye ) );
    MR:= To_Fix( World_Rotation ) * o.auto_rotation;
    for i in reverse 1 .. o.Num_of_points loop
      Rotate( baseobj(i), points(i) );
      points(i).X:= points(i).X + Wce.X;
      points(i).Y:= points(i).Y + Wce.Y;
      points(i).Z:= points(i).Z + Wce.Z;          -- Wx - We
    end loop;
    for i in reverse 1 .. o.Num_of_faces loop
      Rotate( normals(i), RotNormals(i) );
    end loop;
    Project_only(o);
  end Project;

Procedure Sort_faces(o: in out object_3D) renames
  Engine_3D.Sorting.Sort_faces;

Procedure Find_light_direction( Pbeg, Pend: Point; dir: out Vector3 ) is
  A,B,E: vector3;
  begin
    E:= To_RealPoint(Pend);
    B:= To_RealPoint(Pbeg);
    A := E - B;                -- vector from lightsource to lightdest
    dir:= (1.0/Norm(A)) * A;
  end Find_light_direction;

-------------------------
-- Drawing of polygons --
-------------------------

-- Procedures are sorted so: colour/texture pairs with same name;
-- Gouraud-shaded pair, flat-shaded pair, Phong-shaded pair

  -- array types for communication object - face drawing
  
  type Map_idx_pair is record U,V: Natural; end record;
  type Map_idx_pair_array is Array(natural range <>) of Map_idx_pair;
  type Color_type_array   is Array(natural range <>) of Colors.Color_Index;
  type Intensity_array    is Array(natural range <>) of Intensity;
  type Z_val_array        is Array(natural range <>) of Real;

  type scan_direction is (horizontal, vertical);

  -- Pixel rounding policy.
  --   1. ceiling for min, floor for max leaves holes
  --   2. floor everywhere overlaps but ensures no hole

  function Poly_min_rounding(f:fix) return integer renames Floor;
  function Poly_max_rounding(f:fix) return integer renames Floor;
  function Line_min_rounding(f:fix) return integer renames Floor;
  function Line_max_rounding(f:fix) return integer renames Floor;

  function Subpixelling( cpix: Integer; cexa: Fix ) return Real is
  begin
    return   Real( cpix+1 ) -- fake ceiling... (for 2.)
           - Real( cexa ) * iv_fl_fix_one;
  end;

  procedure Prepare_poly_scan(
    edges              :     Fixed_ScrPoint_array;
    direction          :     scan_direction;
    Wmin, Wmax         : out integer; -- X or Y
    poly_beg, poly_end : out Fix_Array;
    off_screen         : out boolean) is

    coord: fix;
  begin
    for i in edges'range loop
      case direction is
        when horizontal => coord:= edges(i).X;
        when vertical =>   coord:= edges(i).Y;
      end case;
      if i=1 then
        Wmin:= Poly_min_rounding(coord);
        Wmax:= Poly_max_rounding(coord);
      else
        if Poly_min_rounding(coord) < Wmin then
          Wmin:= Poly_min_rounding(coord);
        end if;
        if Poly_max_rounding(coord) > Wmax then
          Wmax:= Poly_max_rounding(coord);
        end if;
      end if;
    end loop;
    case direction is
      when horizontal =>
        Wmin := max(XLeftClip,  Wmin );
        Wmax := min(XRightClip, Wmax );
        off_screen:= Wmin>Xres or Wmax<0;
      when vertical =>
        Wmin := max(YTopClip, Wmin );
        Wmax := min(YBotClip, Wmax );
        off_screen:= Wmin>Yres or Wmax<0;
    end case;
    if off_screen then return; end if; -- poly completely off screen

    for w in Wmin..Wmax loop
      poly_beg(w) := integer'last;
      poly_end(w) := integer'first;
    end loop;
  end Prepare_poly_scan;
  
Procedure GouraudPolygon(edges : Fixed_ScrPoint_array;
                         colors: Color_type_array
--                         buffer: out Screen_Buffer) is
                         ) is
  Ymin, Ymax : integer;
  poly_beg, poly_end : Fix_Array(0..Yres);
  colo_beg, colo_end : Array(0..Yres) of Graphics.Colors.Color_Index; -- not yet subpixeled...
  off_screen       : boolean;
  
  Procedure ScanPolySide(P1,P2: Fixed_ScrPoint; C1,C2 : Graphics.Colors.Color_Index) is
    use type Graphics.Colors.Color_Index;
    DeltaX, Delcol : fix;
    Xpos, Colo: fix;
    Yspan: Integer; -- in pixels (rounded)
    Y1pix, Y2pix: Integer; -- pixel points for Y1s, Y2s
    X1s, X2s, Y1s, Y2s: fix;
    C1s, C2s, C: Graphics.Colors.Color_Index;
    begin
      if P2.Y > P1.Y then       -- make sure Y1s is top point
        X1s:= P1.X; X2s:= P2.X;
        Y1s:= P1.Y; Y2s:= P2.Y;
        C1s:= C1;   C2s:= C2;
      else
        X1s:= P2.X; X2s:= P1.X;
        Y1s:= P2.Y; Y2s:= P1.Y;
        C1s:= C2;   C2s:= C1;
      end if;

      Y1pix:= Poly_min_rounding(Y1s);
      Y2pix:= Poly_max_rounding(Y2s);
      Yspan:= Y2pix-Y1pix;
      if Yspan<=0 then return; end if;

      Xpos   := X1s;
      DeltaX := (X2s-X1s) / Yspan;
      Colo   := fix(C1s) * fix_one;
      Delcol := ((fix(C2s)-fix(C1s)) * fix_one) / Yspan;

      -- Clipping --
      declare Cut: Integer; -- How many pixels to cut at ends
      begin
        Cut:= YTopClip - Y1pix;
        if Cut > 0 then -- Clip at end #1
          Y1pix:= Y1pix + Cut;
          Y1s  := Y1s   + Cut * fix_one;
          Xpos:= Xpos + DeltaX * fix(Cut);
          Colo:= Colo + Delcol * fix(Cut);
        end if;
        Cut:= Y2pix - YBotClip;
        if Cut > 0 then -- Clip at end #2
          Y2pix:= Y2pix - Cut;
        end if;
      end;
      -- Clipping now prepared --

      for y in Y1pix .. Y2pix loop
       C:= Graphics.Colors.Color_Index(Colo / fix_one);
       if Xpos < poly_beg(y) then
         poly_beg(y):= Xpos;
         colo_beg(y):= C;
       end if;
       if Xpos > poly_end(y) then
         poly_end(y):= Xpos;
         colo_end(y):= C;
       end if;
       Xpos := Xpos + DeltaX;
       Colo := Colo + Delcol;
      end loop;
    end ScanPolySide;

  begin
    Prepare_poly_scan(
      edges, vertical,
      Ymin, Ymax, poly_beg, poly_end, off_screen );
    if off_screen then return; end if; -- poly completely off screen

    declare prev_i: Natural;
    begin
      for i in reverse 1 .. edges'last loop
        if i=1 then prev_i:= edges'last; else prev_i:= i-1; end if;
        ScanPolySide( edges(prev_i), edges(i), colors(prev_i), colors(i) );
      end loop;
    end;
    -- all sides are now scanned

    for y in Ymin .. Ymax loop
      Gouraud_Hor_line(
--        buffer,
        Line_min_rounding(poly_beg(y)),
        Line_max_rounding(poly_end(y)), y,
        colo_beg(y),colo_end(y)
      );
    end loop;

  end GouraudPolygon;

  subpixel: constant boolean:= true;

  -- Texture-Gouraud
  Procedure GouraudPolygon(
            edges : Fixed_ScrPoint_array;
            texcoo: Map_idx_pair_array;
            Zes   : Z_val_array;
            ints  : Intensity_array;
            texture: Texture_map;
            map_mode : texture_mapping_mode;
            rep_U, rep_V: positive
--            buffer: out Screen_Buffer) is
            ) is

    -- Internal to GouraudPolygon: draw with horizontal lines

    Procedure HLined_GouraudPolygon is

      Ymin, Ymax : integer;
      poly_beg, poly_end : Fix_Array(0..Yres);
      int_beg,  int_end :  Array(0..Yres) of intensity; -- not subpixeled...
      Tex_U1, Tex_V1, Tex_U2, Tex_V2 : Array(0..Yres) of sfix;
      fTex_UP1, fTex_VP1, fTex_UP2, fTex_VP2 : Array(0..Yres) of Real;
      iv_Z1Y, iv_Z2Y: Array(0..Yres) of Real;
      off_screen: boolean;

  Procedure ScanPolySide(P1,P2: Fixed_ScrPoint; E1,E2: Map_idx_pair;
                         Z1,Z2: Real;  i1,i2: intensity) is

    DeltaX, Xpos: fix;
    DeltaU, DeltaV, Deltai, Uposfixed, Vposfixed, ipos: sfix;

    UP, VP, DeltaUP, DeltaVP, iv_Z2s, iv_Z, Delta_iv_Z, fl_iv_Yspan: Real;

    X1s, X2s, Y1s, Y2s: fix;
    Yspan: Integer; -- in pixels (rounded)
    Y1pix, Y2pix: Integer; -- pixel points for Y1s, Y2s
    U1s, V1s, U2s, V2s: Integer; -- end pixels in textures
    i1s, i2s: intensity;
    begin
      if P2.Y > P1.Y then       -- make sure Y1s is top point
        X1s:= P1.X; X2s:= P2.X;
        Y1s:= P1.Y; Y2s:= P2.Y;
        U1s:= E1.U; U2s:= E2.U;
        V1s:= E1.V; V2s:= E2.V;
        i1s:= i1; i2s:= i2;
      else
        X1s:= P2.X; X2s:= P1.X;
        Y1s:= P2.Y; Y2s:= P1.Y;
        U1s:= E2.U; U2s:= E1.U;
        V1s:= E2.V; V2s:= E1.V;
        i1s:= i2; i2s:= i1;
      end if;

      Y1pix:= Poly_min_rounding(Y1s);
      Y2pix:= Poly_max_rounding(Y2s);
      Yspan  := (Y2pix-Y1pix); -- type: integer
      if Yspan<=0 then return; end if;

      Xpos := X1s;
      ipos := sfix(i1s) * sfix_one;

      DeltaX := (X2s-X1s) / Yspan; -- type: fix
      Deltai := (sfix(i2s-i1s) * sfix_one) / sfix(Yspan); -- type: sfix

      case map_mode is
        when affine_y_affine_x =>

        Uposfixed := sfix(U1s) * sfix_one;
        Vposfixed := sfix(V1s) * sfix_one;

        if subpixel then -- Subpixel correction for poly scan
          declare correction: Real:= Subpixelling( Y1pix, Y1s );
          begin
            DeltaU := (sfix(U2s-U1s) * sfix_one * fix_one) / sfix(Y2s-Y1s);
            DeltaV := (sfix(V2s-V1s) * sfix_one * fix_one) / sfix(Y2s-Y1s);
            Uposfixed := Uposfixed + sfix(Real(DeltaU) * correction);
            Vposfixed := Vposfixed + sfix(Real(DeltaV) * correction);
          end;
        else
          DeltaU := (sfix(U2s-U1s) * sfix_one) / sfix(Yspan);
          DeltaV := (sfix(V2s-V1s) * sfix_one) / sfix(Yspan);
        end if;

        -- Clipping --
        declare Cut: Integer; -- How many pixels to cut at ends
        begin
          Cut:= YtopClip - Y1pix;
          if Cut > 0 then -- Clip at end #1
            Y1pix:= Y1pix + Cut;
            Xpos      := Xpos + DeltaX * fix(Cut);
            Uposfixed := UposFixed + DeltaU * sfix(Cut);
            Vposfixed := VposFixed + DeltaV * sfix(Cut);
            ipos      := ipos      + Deltai * sfix(Cut);
          end if;
          Cut:= Y2pix - YBotClip;
          if Cut > 0 then -- Clip at end #2
            Y2pix:= Y2pix - Cut;
          end if;
        end;
        -- Clipping now prepared --

        for y in Y1pix .. Y2pix loop
         if Xpos < poly_beg(y) then
           poly_beg(y):= Xpos;
           int_beg(y):= intensity(ipos / sfix_one);
           Tex_U1(y):= UposFixed;
           Tex_V1(y):= VposFixed;
         end if;
         if Xpos > poly_end(y) then
           poly_end(y):= Xpos;
           int_end(y):= intensity(ipos / sfix_one);
           Tex_U2(y):= UposFixed;
           Tex_V2(y):= VposFixed;
         end if;
         Xpos      := Xpos      + DeltaX;
         Uposfixed := UposFixed + DeltaU;
         Vposfixed := VposFixed + DeltaV;
         ipos := ipos + Deltai;
        end loop;

        when npersp_y_affine_x | npersp_y_npersp_x =>

        if P2.Y > P1.Y then
          iv_Z:= 1.0 / Z1; iv_Z2s:= 1.0 / Z2;
        else
          iv_Z:= 1.0 / Z2; iv_Z2s:= 1.0 / Z1;
        end if;

        UP:= Real(U1s) * iv_Z;
        VP:= Real(V1s) * iv_Z;
        fl_iv_Yspan:= 1.0 / Real(Yspan);
        Delta_iv_Z := (iv_Z2s - iv_Z) * fl_iv_Yspan;

        if subpixel then -- Subpixel correction for poly scan
          declare
            correction: Real:= Subpixelling( Y1pix, Y1s );
            iv_exact_span: Real:= fl_fix_one / Real(Y2s-Y1s); -- unit:1/pixels
          begin
            DeltaUP := (Real(U2s) * iv_Z2s - Real(U1s) * iv_Z) * iv_exact_span;
            DeltaVP := (Real(V2s) * iv_Z2s - Real(V1s) * iv_Z) * iv_exact_span;
            UP:= UP + DeltaUP * correction;
            VP:= VP + DeltaVP * correction;
          end;
        else
          DeltaUP := (Real(U2s) * iv_Z2s - Real(U1s) * iv_Z) * fl_iv_Yspan;
          DeltaVP := (Real(V2s) * iv_Z2s - Real(V1s) * iv_Z) * fl_iv_Yspan;
        end if;

        -- Clipping --
        declare Cut: Integer; -- How many pixels to cut at ends
        begin
          Cut:= YtopClip - Y1pix;
          if Cut > 0 then -- Clip at end #1
            Y1pix:= Y1pix + Cut;
            Xpos:= Xpos + DeltaX * fix(Cut);
            UP:=         UP        + DeltaUP    * Real(Cut);
            VP:=         VP        + DeltaVP    * Real(Cut);
            iv_Z:=       iv_Z      + Delta_iv_Z * Real(Cut);
            ipos := ipos + Deltai     * sfix(Cut);
          end if;
          Cut:= Y2pix - YBotClip;
          if Cut > 0 then -- Clip at end #2
            Y2pix:= Y2pix - Cut;
          end if;
        end;
        -- Clipping now prepared --

        for y in Y1pix .. Y2pix loop
         if Xpos < poly_beg(y) then
           poly_beg(y):= Xpos;
           int_beg(y):= intensity(ipos / sfix_one);
           fTex_UP1(y):= UP;
           fTex_VP1(y):= VP;
           iv_Z1Y(y):= iv_Z;
         end if;
         if Xpos > poly_end(y) then
           poly_end(y):= Xpos;
           int_end(y):= intensity(ipos / sfix_one);
           fTex_UP2(y):= UP;
           fTex_VP2(y):= VP;
           iv_Z2Y(y):= iv_Z;
         end if;
         Xpos:= Xpos + DeltaX;
         UP  := UP   + DeltaUP;
         VP  := VP   + DeltaVP;
         iv_Z:= iv_Z + Delta_iv_Z;
         ipos:= ipos + Deltai;
        end loop;

        when auto | affine_y_npersp_x => null; -- auto / x.y are nonsense here

      end case;

    end ScanPolySide;

  begin
    Prepare_poly_scan(
      edges, vertical,
      Ymin, Ymax, poly_beg, poly_end, off_screen );
    if off_screen then return; end if; -- poly completely off screen

    declare prev_i: Natural;
    begin
      for i in reverse 1 .. edges'last loop
        if i=1 then prev_i:= edges'last; else prev_i:= i-1; end if;
        ScanPolySide( edges (prev_i), edges (i),
                      texcoo(prev_i), texcoo(i),
                      Zes   (prev_i), Zes   (i),
                      ints  (prev_i), ints  (i) );
      end loop;
    end;
    -- all sides are now scanned

    Set_current_texture( texture, rep_U, rep_V );

    case map_mode is

      when affine_y_affine_x =>

        for y in Ymin .. Ymax loop
          declare
            X1pix, X2pix: Integer;     -- pixel points for each scanline
            SCU1,SCU2,SCV1,SCV2: sfix;
            correction, slope: Real;
          begin
            X1pix:= Line_min_rounding(poly_beg(y));
            X2pix:= Line_max_rounding(poly_end(y));
            SCU1:= Tex_U1(y); SCV1:= Tex_V1(y);
            SCU2:= Tex_U2(y); SCV2:= Tex_V2(y);

            -- Subpixel correction for line scan
            if subpixel and then X1pix/=X2pix then
              correction:= Subpixelling( X1pix, poly_beg(y) );
              slope:= fl_fix_one / Real(poly_end(y)-poly_beg(y));
              SCU1:= sfix(Real(SCU1) + Real(SCU2-SCU1) * correction * slope );
              SCV1:= sfix(Real(SCV1) + Real(SCV2-SCV1) * correction * slope );
            end if;
            
            Affine_TextureMap_Hor_Line(
--               buffer,
               X1pix, X2pix, y,
               SCU1, SCV1, SCU2, SCV2,
               int_beg(y), int_end(y) );
          end;
        end loop;

      when npersp_y_affine_x =>
        for y in Ymin .. Ymax loop

          declare
            Zbeg, Zend: Real;
            X1pix, X2pix: Integer;     -- pixel points for each scanline
            SCU1,SCU2,SCV1,SCV2: sfix;
            correction, slope: Real;
          begin
            Zbeg:= fl_sfix_one/iv_Z1Y(y);
            Zend:= fl_sfix_one/iv_Z2Y(y);
            X1pix:= Line_min_rounding(poly_beg(y));
            X2pix:= Line_max_rounding(poly_end(y));
            SCU1:= sfix(Zbeg*fTex_UP1(y));
            SCV1:= sfix(Zbeg*fTex_VP1(y));
            SCU2:= sfix(Zend*fTex_UP2(y));
            SCV2:= sfix(Zend*fTex_VP2(y));

            -- Subpixel correction for line scan
            if subpixel and then X1pix/=X2pix then
              correction:= Subpixelling( X1pix, poly_beg(y) );
              slope:= fl_fix_one / Real(poly_end(y)-poly_beg(y));
              SCU1:= sfix(Real(SCU1) + Real(SCU2-SCU1) * correction * slope );
              SCV1:= sfix(Real(SCV1) + Real(SCV2-SCV1) * correction * slope );
            end if;

            Affine_TextureMap_Hor_Line(
--               buffer,
               X1pix, X2pix, y,
               SCU1, SCV1, SCU2, SCV2,
               int_beg(y), int_end(y) );
          end;
        end loop;

      when npersp_y_npersp_x =>

        for y in Ymin .. Ymax loop
          declare
            X1pix, X2pix: Integer;     -- pixel points for each scanline
            SCU1,SCU2,SCV1,SCV2: Real;
            correction, slope: Real;
          begin
            X1pix:= Line_min_rounding(poly_beg(y));
            X2pix:= Line_max_rounding(poly_end(y));
            SCU1:= fTex_UP1(y);
            SCV1:= fTex_VP1(y);
            SCU2:= fTex_UP2(y);
            SCV2:= fTex_VP2(y);

            -- Subpixel correction for line scan
            if subpixel and then X1pix/=X2pix then
              correction:= Subpixelling( X1pix, poly_beg(y) );
              slope:= fl_fix_one / Real(poly_end(y)-poly_beg(y));
              SCU1:= SCU1 + (SCU2-SCU1) * correction * slope;
              SCV1:= SCV1 + (SCV2-SCV1) * correction * slope;
            end if;

            TextureMap_Hor_Line(
--              buffer,
              X1pix, X2pix, y,
              SCU1, SCV1, SCU2, SCV2,
              iv_Z1Y(y),  iv_Z2Y(y),
              int_beg(y), int_end(y) );
          end;
        end loop;

      when auto | affine_y_npersp_x => null; -- auto / x.y are nonsense here

    end case;

  end Hlined_GouraudPolygon;

 -- Internal to GouraudPolygon: draw with vertical lines

 Procedure VLined_GouraudPolygon is

  Xmin, Xmax : integer;
  poly_beg, poly_end : Fix_Array(0..Xres);
  int_beg,  int_end :  Array(0..Xres) of intensity; -- not yet subpixeled...
  fTex_UP1, fTex_VP1, fTex_UP2, fTex_VP2 : Array(0..Xres) of Real;
  iv_Z1X, iv_Z2X: Array(0..Xres) of Real;
  off_screen: boolean;

  Procedure ScanPolySide(P1,P2: Fixed_ScrPoint; E1,E2: Map_idx_pair;
                         Z1,Z2: Real;  i1,i2: intensity) is

    Deltai, ipos: sfix;
    UP, VP, DeltaUP, DeltaVP, iv_Z2s, iv_Z, Delta_iv_Z, fl_iv_Xspan: Real;
    Xspan: Integer; -- in pixels (rounded)
    X1pix, X2pix: Integer; -- pixel points for X1s, X2s
    X1s, X2s, Y1s, Y2s, Ypos: fix;
    DeltaY: fix;
    U1s, V1s, U2s, V2s: Integer;  -- end pixels in textures
    i1s, i2s: intensity;
    begin
      if P2.X > P1.X then       -- make sure X1s is left point
        X1s:= P1.X; X2s:= P2.X;
        Y1s:= P1.Y; Y2s:= P2.Y;
        U1s:= E1.U; U2s:= E2.U;
        V1s:= E1.V; V2s:= E2.V;
        i1s:= i1; i2s:= i2;
      else
        X1s:= P2.X; X2s:= P1.X;
        Y1s:= P2.Y; Y2s:= P1.Y;
        U1s:= E2.U; U2s:= E1.U;
        V1s:= E2.V; V2s:= E1.V;
        i1s:= i2; i2s:= i1;
      end if;

      X1pix:= Poly_min_rounding(X1s);
      X2pix:= Poly_max_rounding(X2s);
      Xspan:= X2pix-X1pix;
      if Xspan<=0 then return; end if;

      Ypos := Y1s;
      ipos := sfix(i1s) * sfix_one;

      DeltaY := (Y2s-Y1s) / Xspan;
      Deltai := (sfix(i2s-i1s) * sfix_one) / sfix(Xspan);

      if P2.X > P1.X then
        iv_Z:= 1.0 / Z1; iv_Z2s:= 1.0 / Z2;
      else
        iv_Z:= 1.0 / Z2; iv_Z2s:= 1.0 / Z1;
      end if;

      UP:= Real(U1s) * iv_Z;
      VP:= Real(V1s) * iv_Z;
      fl_iv_Xspan:= 1.0 / Real(Xspan);
      Delta_iv_Z := (iv_Z2s - iv_Z) * fl_iv_Xspan;

        if subpixel then -- Subpixel correction for poly scan
          declare
            correction: Real:= Subpixelling( X1pix, X1s );
            iv_exact_span: Real:= fl_fix_one / Real(X2s-X1s); -- unit:1/pixels
          begin
            DeltaUP := (Real(U2s) * iv_Z2s - Real(U1s) * iv_Z) * iv_exact_span;
            DeltaVP := (Real(V2s) * iv_Z2s - Real(V1s) * iv_Z) * iv_exact_span;
            UP:= UP + DeltaUP * correction;
            VP:= VP + DeltaVP * correction;
          end;
        else
          DeltaUP := (Real(U2s) * iv_Z2s - Real(U1s) * iv_Z) * fl_iv_Xspan;
          DeltaVP := (Real(V2s) * iv_Z2s - Real(V1s) * iv_Z) * fl_iv_Xspan;
        end if;

        -- Clipping --
        declare Cut: Integer; -- How many pixels to cut at ends
        begin
          Cut:= XLeftClip - X1pix;
          if Cut > 0 then -- Clip at end #1
            X1pix:= X1pix + Cut;
            Ypos:= Ypos + DeltaY     * fix(Cut);
            UP:=   UP   + DeltaUP    * Real(Cut);
            VP:=   VP   + DeltaVP    * Real(Cut);
            iv_Z:= iv_Z + Delta_iv_Z * Real(Cut);
            ipos:= ipos + Deltai     * sfix(Cut);
          end if;
          Cut:= X2pix - XRightClip;
          if Cut > 0 then -- Clip at end #2
            X2pix:= X2pix - Cut;
          end if;
        end;
        -- Clipping now prepared --

      for x in X1pix .. X2pix loop
       if Ypos < poly_beg(X) then
         poly_beg(X):= Ypos;
         int_beg(X):= intensity(ipos / sfix_one);
         fTex_UP1(X):= UP;
         fTex_VP1(X):= VP;
         iv_Z1X(X):= iv_Z;
       end if;
       if Ypos > poly_end(X) then
         poly_end(X):= Ypos;
         int_end(X):= intensity(ipos / sfix_one);
         fTex_UP2(X):= UP;
         fTex_VP2(X):= VP;
         iv_Z2X(X):= iv_Z;
       end if;
       Ypos:= Ypos + DeltaY;
       UP:=   UP   + DeltaUP;
       VP:=   VP   + DeltaVP;
       iv_Z:= iv_Z + Delta_iv_Z;
       ipos:= ipos + Deltai;
      end loop;

    end ScanPolySide;

  begin
    Prepare_poly_scan(
      edges, horizontal,
      Xmin, Xmax, poly_beg, poly_end, off_screen );
    if off_screen then return; end if; -- poly completely off screen

    declare prev_i: Natural;
    begin
      for i in reverse 1 .. edges'last loop
        if i=1 then prev_i:= edges'last; else prev_i:= i-1; end if;
        ScanPolySide( edges (prev_i), edges (i),
                      texcoo(prev_i), texcoo(i),
                      Zes   (prev_i), Zes   (i),
                      ints  (prev_i), ints  (i) );
      end loop;
    end;
    -- all sides are now scanned

    Set_current_texture( texture, rep_U, rep_V );

    for x in Xmin .. Xmax loop
      declare
        Zbeg, Zend: Real;
        Y1pix, Y2pix: Integer;     -- pixel points for each scanline
        SCU1,SCU2,SCV1,SCV2: sfix;
        correction, slope: Real;
      begin
        Zbeg:= fl_sfix_one/iv_Z1X(x);
        Zend:= fl_sfix_one/iv_Z2X(x);
        Y1pix:= Line_min_rounding(poly_beg(x));
        Y2pix:= Line_max_rounding(poly_end(x));
        SCU1:= sfix(Zbeg*fTex_UP1(x));
        SCV1:= sfix(Zbeg*fTex_VP1(x));
        SCU2:= sfix(Zend*fTex_UP2(x));
        SCV2:= sfix(Zend*fTex_VP2(x));

        -- Subpixel correction for line scan
        if subpixel and then Y1pix/=Y2pix then
          correction:= Subpixelling( Y1pix, poly_beg(x) );
          slope:= fl_fix_one / Real(poly_end(x)-poly_beg(x));
          SCU1:= sfix(Real(SCU1) + Real(SCU2-SCU1) * correction * slope );
          SCV1:= sfix(Real(SCV1) + Real(SCV2-SCV1) * correction * slope );
        end if;
        Affine_TextureMap_Ver_Line(
--          buffer,
          x, Y1pix, Y2pix,
          SCU1, SCV1, SCU2, SCV2,
          int_beg(x), int_end(x));
      end;
    end loop;

   end VLined_GouraudPolygon;

 begin -- Textured Gouraud Polygon

   case map_mode is
     when affine_y_npersp_x =>  VLined_GouraudPolygon;
     when others =>             HLined_GouraudPolygon;
   end case;

 end GouraudPolygon;

-- Lambert

Procedure Polygon(edges : Fixed_ScrPoint_array;
                  C     : Colors.Color_Index
--                  buffer: out Screen_Buffer) is
                  ) is
  colors: Color_type_array(edges'range):= (others=> C); -- all the same
  begin
--    GouraudPolygon(edges, colors, buffer);
    GouraudPolygon(edges, colors);
  end;

Procedure Polygon(edges : Fixed_ScrPoint_array;
                  texcoo: Map_idx_pair_array;
                  Zes   : Z_val_array;
                  i: intensity;
                  texture: Texture_map;
                  map_mode : texture_mapping_mode;
                  rep_U, rep_V: positive
--                  buffer: out Screen_Buffer) is
                  ) is

  ints: Intensity_array(edges'range):= (others => i); -- same intensity 

  begin GouraudPolygon(edges, texcoo, Zes, ints,
                       texture, map_mode, rep_U, rep_V
--                       buffer);
                       );
  end;

-- Environ mapped

Procedure EnvMapPolygon(edges : Fixed_ScrPoint_array;
                        envcoo: Map_idx_pair_array;
                        Cmin, Num_of_shades: Colors.Color_Index
--                        buffer: out Screen_Buffer) is
                        ) is
  Ymin, Ymax : integer;
  poly_beg, poly_end : Fix_Array(0..Yres);
  Env_U1, Env_V1, Env_U2, Env_V2 : Array(0..Yres) of Phong_index;
  off_screen: boolean;

  Procedure ScanPolySide(P1,P2: Fixed_ScrPoint; E1,E2: Map_idx_pair) is
    DeltaX, DeltaU, DeltaV : fix;
    Uposfixed, Vposfixed: fix;
    Yspan: Integer; -- in pixels (rounded)
    Y1pix, Y2pix: Integer; -- pixel points for Y1s, Y2s
    X1s, X2s, Y1s, Y2s, Xpos: fix;
    U1s, V1s, U2s, V2s: Integer;  -- end pixels in textures
    begin
      if P2.Y > P1.Y then       -- make sure Y1s is top point
        X1s:= P1.X; X2s:= P2.X;
        Y1s:= P1.Y; Y2s:= P2.Y;
        U1s:= E1.U; U2s:= E2.U;
        V1s:= E1.V; V2s:= E2.V;
      else
        X1s:= P2.X; X2s:= P1.X;
        Y1s:= P2.Y; Y2s:= P1.Y;
        U1s:= E2.U; U2s:= E1.U;
        V1s:= E2.V; V2s:= E1.V;
      end if;

      Y1pix:= Poly_min_rounding(Y1s);
      Y2pix:= Poly_max_rounding(Y2s);
      Yspan:= Y2pix-Y1pix;
      if Yspan<=0 then return; end if;

      Xpos := X1s;
      Uposfixed := U1s * fix_one;
      Vposfixed := V1s * fix_one;

      DeltaX := (X2s-X1s) / Yspan;
      DeltaU := ((U2s-U1s) * fix_one) / Yspan;
      DeltaV := ((V2s-V1s) * fix_one) / Yspan;

      -- Clipping --
      declare Cut: Integer; -- How many pixels to cut at ends
      begin
        Cut:= YtopClip - Y1pix;
        if Cut > 0 then -- Clip at end #1
          Y1pix:= Y1pix + Cut;
          Xpos := Xpos + DeltaX * fix(Cut);
          Uposfixed := UposFixed + DeltaU * fix(Cut);
          Vposfixed := VposFixed + DeltaV * fix(Cut);
        end if;
        Cut:= Y2pix - YBotClip;
        if Cut > 0 then -- Clip at end #2
          Y2pix:= Y2pix - Cut;
        end if;
      end;
      -- Clipping now prepared --

      for y in Y1pix .. Y2pix loop
       if Xpos < poly_beg(y) then
         poly_beg(y):= Xpos;
         Env_U1(y):= UposFixed / fix_one;
         Env_V1(y):= VposFixed / fix_one;
       end if;
       if Xpos > poly_end(y) then
         poly_end(y):= Xpos;
         Env_U2(y):= UposFixed / fix_one;
         Env_V2(y):= VposFixed / fix_one;
       end if;
       Xpos      := Xpos + DeltaX;
       Uposfixed := UposFixed + DeltaU;
       Vposfixed := VposFixed + DeltaV;
      end loop;
    end ScanPolySide;

  begin
    Prepare_poly_scan(
      edges, vertical,
      Ymin, Ymax, poly_beg, poly_end, off_screen );
    if off_screen then return; end if; -- poly completely off screen

    declare prev_i: Natural;
    begin
      for i in reverse 1 .. edges'last loop
        if i=1 then prev_i:= edges'last; else prev_i:= i-1; end if;
        ScanPolySide( edges (prev_i), edges (i),
                      envcoo(prev_i), envcoo(i) );
      end loop;
    end;
    -- all sides are now scanned

    for y in Ymin .. Ymax loop
      EnvMap_Hor_Line(
--        buffer,
        Line_min_rounding(poly_beg(y)),
        Line_max_rounding(poly_end(y)),
        y,
        Env_U1(y), Env_V1(y),
        Env_U2(y), Env_V2(y),
        Cmin,
        Byte (Num_of_shades));
    end loop;

  end EnvMapPolygon;

  -- The following function selects the best available scanline
  -- in order to have the fastest but also a correct
  -- texture mapping.

  function Mapping_selection( normal: Fixed_vectT )
    return texture_mapping_mode is

    tol_XY: constant fix:= fix( 0.029 * fl_fix_one); -- max  0.035
    tol_Z : constant fix:= fix(-0.998 * fl_fix_one); -- max -0.998

  begin
    if         normal.Z   < tol_Z  then  return affine_y_affine_x; -- ~facing
    elsif abs( normal.X ) < tol_XY then  return npersp_y_affine_x; -- ~floor
    elsif abs( normal.Y ) < tol_XY then  return affine_y_npersp_x; -- ~wall
    else                                 return npersp_y_npersp_x;
    end if;
  end Mapping_selection;

  pragma Inline(Mapping_selection);

  -------------------------------------
  -- The Object_3D drawing procedure --
  -------------------------------------
  fix_two: constant:= fix_one * 2;
  fix_0_5: constant:= fix_one / 2;

  neutral_Z_shade: constant:= 0.5;

  Procedure Draw -- internal
--    (buffer        : out Screen_Buffer;
    (o             : in out Object_3D;
     surf_select   : in  surface_select;
     map_mode      : in  texture_mapping_mode;
     shading       : in  shading_mode;
     do_Z_shading  : in  boolean;
     minZ, maxZ    : in  integer;
     clip          : in  t_rect ) is

  Z_shade : Real;
  N       : Vector3;
  int: intensity;
  num_of_shades: Colors.Color_Index;
  f_num_of: Real;
  selected_map_mode: texture_mapping_mode;

  type dist_mode is (d2point, d2plane); -- radial light for Gouraud / Lambert

  dist2_corr: constant:= 1.0 / (fl_screen_virtual_size**2);

  function N_dot_with_lights(pn: positive; dm:dist_mode) return Real is
    d: Real:= Z_shade - neutral_Z_shade;
    l, dist2, dot, 
    dist2_to_plane, dist2_to_point: Real;
    sep: Vector3;
    rpn: RealPoint:= To_RealPoint(o.points(pn));
    begin
      for i in reverse 1 .. parallel_lights loop
        l:= - (N * pll_lght(i)) *  parallel_light_force(i);
        if l > 0.0 then d:= d + l; end if; -- cannot substract light!
      end loop;
      for i in reverse 1 .. radial_lights loop
        sep:= rpn-rdl_lght(i); -- from light source to point on plane
        dot:= - N*sep; -- distance to plane _and_ projection of sep over N
        dist2_to_plane:= dot ** 2;
        dist2:= dist2_to_plane * dist2_corr;
        case dm is
         when d2point=>
          dist2_to_point:= Norm2(sep);
          if dist2_to_point > 0.0 then
           dot:= dot / Sqrt(dist2_to_point); -- norm. light vect * N
           l:= radial_light_force(i) * dot;
          else
           l:= radial_light_force(i); -- undecidable
          end if;
         when d2plane=>
          l:= radial_light_force(i);
        end case;
        if dist2 > 1.0 then
          l:= l / dist2;
        end if;
        if l > 0.0 then d:= d + l; end if; -- cannot substract light!
      end loop;

      if d > 1.0 then d:= 1.0; elsif d < 0.0 then d:= 0.0; end if;
      return d;
    end N_dot_with_lights;

  pragma Inline(N_dot_with_lights); -- some KB more but FPS too...

  procedure Draw_face(i, polynr: Integer; the_face: Face) is
     use type Graphics.Colors.Color_Index;

    textured: constant boolean:=
      (the_face.textured and surf_select /= colors_only)
      or surf_select = textures_only;

    edges     : Fixed_ScrPoint_array(1..4);
    last_edge : Natural;
    colors    : Color_type_array(edges'range);
    dots      : array(edges'range) of Real;
    Zes       : Z_val_array(edges'range);
    ints      : Intensity_array(edges'range);
    UVes      : Map_idx_pair_array(edges'range);
    FN        : Fixed_point;
    maxx, maxy: Natural;
    P         : Index_array(1..4);  -- compact array of indices of the edges
    subtype t_quadri_edge is Natural range 1..4;
    quadri_edge:  array(edges'range) of t_quadri_edge;

    -- return interpolated values, parameter being a float in [0;1]

    function InterCol(f01:Real) return Graphics.Colors.Color_Index is
      begin return the_face.color_min + Graphics.Colors.Color_Index(f01 * f_num_of);
      end;

    function InterInt(f01:Real) return intensity is
      begin return Intensity(the_face.intens_min + Integer(f01 * f_num_of));
      end;

    begin

      last_edge:= 0;
      for qe in edges'range loop
        if the_face.P(qe) /= 0 then
          last_edge:= last_edge + 1;
          quadri_edge(last_edge):= qe; -- if triangle, "map" edge on a quadri
          P(last_edge):= the_face.P(qe);
          edges(last_edge) := o.projected( P(last_edge) );
        end if;
      end loop;

      -- Hidden face removal using Z-comp of oriented orthogonal to 2D-polygon

      if (edges(3).Y/fix_one - edges(1).Y/fix_one)*(edges(2).X/fix_one - edges(3).X/fix_one) >=
         (edges(3).X/fix_one - edges(1).X/fix_one)*(edges(2).Y/fix_one - edges(3).Y/fix_one) and then

      ( (last_edge = 3) or else

        (
         (edges(1).Y/fix_one - edges(3).Y/fix_one)*(edges(4).X/fix_one - edges(1).X/fix_one) >=
         (edges(1).X/fix_one - edges(3).X/fix_one)*(edges(4).Y/fix_one - edges(1).Y/fix_one) and then

         (edges(4).Y/fix_one - edges(2).Y/fix_one)*(edges(3).X/fix_one - edges(4).X/fix_one) >=
         (edges(4).X/fix_one - edges(2).X/fix_one)*(edges(3).Y/fix_one - edges(4).Y/fix_one) and then

         (edges(2).Y/fix_one - edges(4).Y/fix_one)*(edges(1).X/fix_one - edges(2).X/fix_one) >=
         (edges(2).X/fix_one - edges(4).X/fix_one)*(edges(1).Y/fix_one - edges(2).Y/fix_one) 
        )    -- 5.IV.1999 We test all corners because of almost-flat things
      )
          then

        return;
      end if;

      if textured then -- specific things to prepare for textures
        f_num_of:= Real( the_face.intens_max - the_face.intens_min );

        if map_mode = auto then
          selected_map_mode:= Mapping_selection( o.RotNormals(polynr) );
          if scanline_profiling then
            Scanline_profiler.Add_one_count( selected_map_mode );
          end if;
        else
          selected_map_mode:= map_mode;
        end if;

        maxx:= (2 ** the_Face.texture.x_bits)  -1;
        maxy:= (2 ** the_Face.texture.y_bits)  -1;

        if selected_map_mode /= affine_y_affine_x then
          for e in reverse 1..last_edge loop
            Zes(e):= Real( o.points(P(e)).Z / fix_one );
          end loop;
        end if;

        for e in reverse 1..last_edge loop
          -- 20.VIII.1999: texture y=0 top, y=maxy bottom like screen bitmap !
          case quadri_edge(e) is
            when 1=> UVes(e):= (0,maxy);     -- bottom, left    4--<--3
            when 2=> UVes(e):= (maxx,maxy);  -- bottom, right   |     |
            when 3=> UVes(e):= (maxx,0);     -- top, right      1-->--2
            when 4=> UVes(e):= (0,0);        -- top, left
          end case;
        end loop;

      else -- non-textured
        num_of_shades:= the_face.color_max-the_face.color_min;
        f_num_of:= Real(num_of_shades);
      end if;

      if do_Z_shading then
        Z_shade := Real( maxZ - Min(maxZ,Max(minZ,o.Center_Z(i))) ) /
                   Real( maxZ - minZ );
      else
        Z_shade := neutral_Z_shade;
      end if;

      case shading is

        when Z_only => -- no light, only distance

            if textured then

             if not do_Z_shading then
               int:= 0; -- true neutral intensity is faster:
               -- avoids intensity map ! InterInt(neutral_Z_shade) = 0
               -- only when int_max = -int_min !
             else
               int:= InterInt(Z_shade);
             end if;

             Polygon(edges(1..last_edge), UVes(1..last_edge),
                     Zes(1..last_edge),
                     int,
                     the_Face.texture.all, selected_map_mode, 
                     the_face.rep_U, the_face.rep_V
--                     buffer);
                     );
            else
             Polygon(edges(1..last_edge), Intercol(Z_shade)
--             buffer);
             );
            end if;

        when Lambert =>

            -- Lambert flat shading according to moving light source

            N:= iv_fl_fix_one * (
             Real(o.RotNormals(polynr).X),
             Real(o.RotNormals(polynr).Y),
             Real(o.RotNormals(polynr).Z) );

            if textured then
             Polygon(edges(1..last_edge), UVes(1..last_edge),
                     Zes(1..last_edge),
                     InterInt(N_dot_with_lights(P(1), d2plane)),
                     the_Face.texture.all, selected_map_mode, 
                     the_face.rep_U, the_face.rep_V
--                     buffer);
                     );
            else
             Polygon(edges(1..last_edge),
                     Intercol(N_dot_with_lights(P(1), d2plane))
--                     buffer);
                     );
            end if;

        when Gouraud =>

            -- Gouraud shading according to moving light source
            for e in reverse 1..last_edge loop
              PointNormal(o, P(e), N);
              dots(e) := N_dot_with_lights(P(e), d2point);
            end loop;

            if textured then
             for e in reverse 1..last_edge loop
               ints(e):= InterInt(dots(e));
             end loop;
             GouraudPolygon(
                     edges(1..last_edge), UVes(1..last_edge),
                     Zes(1..last_edge),
                     ints(1..last_edge),
                     the_Face.texture.all, selected_map_mode, 
                     the_face.rep_U, the_face.rep_V
--                     buffer);
                     );
            else
             for e in reverse 1..last_edge loop
               colors(e):= Intercol(dots(e));
             end loop;
             GouraudPolygon( edges(1..last_edge), colors(1..last_edge)
--             buffer );
             );
            end if;

        when Phong =>

            -- Environment-mapped Phong

            for e in reverse 1..last_edge loop
              PointNormal(o, P(e), FN);
              UVes(e):=
                 (U=> (Phong_max_idx * FN.X) / fix_two + Phong_max_idx/2,
                  V=> (Phong_max_idx * FN.Y) / fix_two + Phong_max_idx/2);
            end loop;

            if textured then
              null;
            else
              EnvmapPolygon(edges(1..last_edge), UVes(1..last_edge),
                            the_face.color_min, Num_of_shades
--                            buffer);
                            );
            end if;

         end case;

    end Draw_Face;
  pragma Inline(Draw_Face);

  invalid_face_order: exception;
  ordi: integer;
  
  procedure Set_SVGA_Clip is
    begin
      Xleftclip:=  clip.X1;
      Xrightclip:= clip.X2;
      Ytopclip:=   clip.Y1;
      Ybotclip:=   clip.Y2;
    end;

  begin -- Draw
    Set_SVGA_Clip;
    if (not o.sorting_refreshed) or (not o.projection_refreshed) then
      Project(o);
      o.projection_refreshed:= True;
    end if;
    if not o.sorting_refreshed then
      Sort_faces(o);
      o.sorting_refreshed:= True;
    end if;
    for i in reverse 1 .. o.Num_of_faces loop
      ordi:= o.Order(i);
      if ordi not in 1..o.Num_of_faces then
        raise_exception(invalid_face_order'identity,
         o.id &
         " i="&integer'image(i) &
         " o.Order(i)="&integer'image(ordi) &
         " nf="&integer'image(o.Num_of_faces));
      end if;
      if o.visible_face(ordi) then
        case o.portal(ordi).Portal_type is
          when just_a_face => null;
          when mirror      => null;
          when see_through => 
            declare
              bounding, inters: t_rect;
              non_empty: boolean;
            begin
              Find_bounding_box( o,ordi,bounding );
              Intersect( clip,bounding, inters, non_empty );
              if non_empty then
                Draw(
--                     buffer,
                     o.portal(ordi).Connecting.all,
                     surf_select,map_mode,
                     shading,do_Z_shading,minZ,maxZ,
                     inters);
                -- Frame_Rect(buffer,inters.x1,inters.y1,inters.x2,inters.y2,251);
                Set_SVGA_Clip;
              end if;
            end;
        end case;
        if not o.portal(ordi).Invisible then
          Draw_Face(i, ordi, o.Faces(ordi));
        end if;
      end if;
    end loop;
  end Draw;

  -- Exported procedure with main clipping

  Procedure Draw (--buffer        : out Screen_Buffer;
                  o             : in out Object_3D;
                  surf_select   : in  surface_select;
                  map_mode      : in  texture_mapping_mode;
                  shading       : in  shading_mode;
                  do_Z_shading  : in  boolean:= false;
                  minZ, maxZ    : in  integer:= 0
                 ) is
    begin
      o.projection_refreshed:= False; -- main object has to be re-projected
      Draw(
--      buffer,
           o,surf_select,map_mode,
           shading,do_Z_shading,minZ,maxZ,
           main_clipping_rect);
    end Draw;

end Engine_3D;
