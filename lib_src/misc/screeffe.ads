------------------------------------------------------------------------------
--  File:            svgaeffe.ads
--  Description:     - Horizontal and vertical scanlines with special
--                     shading and texture effects (for 3D)
--
--  Date / Version:  9.XII.1999 / 23.VII.1999 / 3.IV.1999
--  Author's e-mail: Gautier.deMontmollin@Maths.UniNe.CH
--
--  Copyright (c) Gautier de Montmollin 1999
--  CH-2000 Neuchatel
--  SWITZERLAND
--  Permission granted to use this software, without any warranty, for any
--  purpose, provided this copyright note remains attached and unmodified.
------------------------------------------------------------------------------

with Ada.Finalization, Ada.Numerics.Generic_Elementary_functions, Interfaces;
with Graphics.Colors;

package Screen.Effects is

  --------------
  -- Clipping --
  --------------
  
  -- NB: 1) these variables must be set !
  --     2) Hor_lines do only X-clipping; Y-clipping is meant
  --        to be done at polygon scanning level. Idem for Y-X

  XLeftClip, XRightClip: Integer;  -- horizontal clipping range
  YTopClip,  YBotClip:   Integer;  -- vertical clipping range

  -------------------------------
  -- Texture, intensity, Phong --
  -------------------------------

  -- Texture maps --
  type Texture_map(x_bits, y_bits: Natural) is limited private;

  -- You must use these procedures to set a texture's contents...
--  procedure Set_Pixel(t: in out Texture_map; x,y: natural; Color: Color_Type);
  procedure Set_Pixel(t: in out Texture_map; x,y: natural; Color: Graphics.Colors.Color_Index);
--  procedure Put_Buffer (Source      : in     Screen_Buffer;
--                        Destination : in out Texture_map);
  procedure Put_Buffer (Destination : in out Texture_map);

  -- Pointers on texture maps:
  type p_Texture_map is access all Texture_map;

  -- The key colour number set to the following variable will
  -- be interpretated as transparent in a texture map:
  transparency_color: Graphics.Colors.Color_Index := 0;

  -- Some famous values...
  Doom_transparency_color: constant:= 247;
  Duke3D_transparency_color: constant:= 255;

  -- Intensity of colours --
  subtype Intensity is integer range -40 .. 40;
  -- 0: neutral; >0: brighter <0: darker
  -- NB: in a RGB "truecolor" context it can tend more in R,G or B...

  -- Intensity mapping --
  --  This is for defining intensities in a poor
  --  256-colour video mode with a palette
  
  type color_degrade_description is record
    c: Graphics.Colors.Color_Index; -- 1st colour index in the d‚grad‚
    n: Intensity;  -- |n|+1= number of colours in the d‚grad‚
  end record;

  type degrade_description is array(natural range <>) of
         color_degrade_description;
 
  -- Some famous mappings...

  Doom_intensities: constant degrade_description:=
   ( (5,-3),    (16,-15), (32,-15), (48,-15),
     (64,-15),  (80,-15), (96,-15), (112,-15), (128,-15),
     (144,-7),  (152,-7), (160,-7), (168,-23), (192,-15),
     (208,-15), (224,-7), (232,-3), (236,-3), (240,-6),
     (250,-4) );
     
  Duke3D_intensities: constant degrade_description:=
   ( (0,31),   (32,31),  (64,15),  (80,15),  (96,31),  (128,15),
     (144,15), (160,31), (192,15), (208,15), (224,15) );
     
  procedure Reset_intensity_map;
  procedure Calculate_intensity_map(d: degrade_description);

  -- Phong map (indices only - the contents are handled by SVGA.Effects) --
  Phong_max_idx: constant:= 255;
  subtype Phong_index is natural range 0..Phong_max_idx;
  
  -- Floating point type used here
  subtype Real is long_float;
  package El_Func is new Ada.Numerics.Generic_Elementary_functions(Real);
  use El_Func;

  -- "hand-made" fixed point type
  subtype  Sfix is Interfaces.Integer_32;
  subtype uSfix is Interfaces.Unsigned_32;
  sfix_bits: constant:= 8;
  sfix_one: constant:= 2**sfix_bits;       -- 1.0 <-> sfix_one
  fl_sfix_one:    constant Real:= Real(sfix_one);
  iv_fl_sfix_one: constant Real:= 1.0 / fl_sfix_one;

  ------------------------------------------------
  -- The special Hor_Line / Ver_Line procedures --
  ------------------------------------------------

  -- NB:  X1<X2 and Y1<Y2 are required (no line in ">=" case)

  -- *** A) Shaded colours ***

  -- Coulour value is interpolated between C1 and C2 :

--  procedure Gouraud_Hor_Line
--     (Buffer : in out Screen_Buffer;
--      X1, X2 : in     Integer;
--      Y      : in     Y_Loc;
--      C1, C2 : in     Color_Type);
  procedure Gouraud_Hor_Line
     (X1, X2 : in     Integer;
      Y      : in     Integer;
      C1, C2 : in     Graphics.Colors.Color_Index);

  -- Environment mapping (pseudo-Phong):

--  procedure EnvMap_Hor_Line
--     (Buffer              : in out Screen_Buffer;
--      X1, X2              : in     Integer;
--      Y                   : in     Y_Loc;
--      U1, V1, U2, V2      : in     Phong_index;
--      Cmin, Num_of_shades : in     Color_type);
  procedure EnvMap_Hor_Line
     (X1, X2              : in     Integer;
      Y                   : in     Integer;
      U1, V1, U2, V2      : in     Phong_index;
      Cmin                : in     Graphics.Colors.Color_Index;
      Num_of_shades       : in     Byte);

  -- *** B) Shaded textures ***

  procedure Set_Current_texture
     (Texture      : in Texture_map;
      rep_U, rep_V : in Positive );   -- texture can be repeated

  -- Texture is linearily mapped (faster but distorted) :

--  procedure Affine_TextureMap_Hor_Line
--     (Buffer         : in out Screen_Buffer;
--      X1, X2         : in     Integer;
--      Y              : in     Y_Loc;
--      U1, V1, U2, V2 : in     Sfix;     -- end points in texture (fxd-pnt)
--      int1, int2     : in     Intensity -- intensity at end points
--     );
  procedure Affine_TextureMap_Hor_Line
     (X1, X2         : in     Integer;
      Y              : in     Integer;
      U1, V1, U2, V2 : in     Sfix;     -- end points in texture (fxd-pnt)
      int1, int2     : in     Intensity -- intensity at end points
     );

--  procedure Affine_TextureMap_Ver_Line
--     (Buffer         : in out Screen_Buffer;
--      X              : in     X_Loc;
--      Y1, Y2         : in     Integer;
--      U1, V1, U2, V2 : in     Sfix;     -- end points in texture (fxd-pnt)
--      int1, int2     : in     Intensity -- intensity at end points
--     );
  procedure Affine_TextureMap_Ver_Line
     (X              : in     Integer;
      Y1, Y2         : in     Integer;
      U1, V1, U2, V2 : in     Sfix;     -- end points in texture (fxd-pnt)
      int1, int2     : in     Intensity -- intensity at end points
     );

  -- Texture is mapped according to perspective :

--  procedure TextureMap_Hor_Line
--     (Buffer             : in out Screen_Buffer;
--      X1, X2             : in     Integer;
--      Y                  : in     Y_Loc;
--      UP1, VP1, UP2, VP2 : in     Real;         -- end 1/points in texture
--      iv_Z1, iv_Z2       : in     Real;         -- end 1/points' Zs
--      int1, int2         : in     Intensity     -- intensity at end points
--     );
  procedure TextureMap_Hor_Line
     (X1, X2             : in     Integer;
      Y                  : in     Integer;
      UP1, VP1, UP2, VP2 : in     Real;         -- end 1/points in texture
      iv_Z1, iv_Z2       : in     Real;         -- end 1/points' Zs
      int1, int2         : in     Intensity     -- intensity at end points
     );

private

   type Texture_map(x_bits, y_bits: Natural) is
     new Ada.Finalization.Limited_Controlled with
      record
        width, height: Natural;
        x_mask, y_mask, x_sfix_mask, y_sfix_mask: Interfaces.Unsigned_32;
        Data: aliased Data_Buffer_Access;
      end record;

   procedure Finalize (TM : in out Texture_map);
   procedure Initialize (TM : in out Texture_map);

pragma Inline(Affine_TextureMap_Ver_Line);
pragma Inline(TextureMap_Hor_Line);
pragma Inline(EnvMap_Hor_Line);

end Screen.Effects;
