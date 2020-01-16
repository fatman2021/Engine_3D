-----------------------------------------------------------------------------
--  File: svgaeffe.adb; see specification (svgaeffe.ads)
-----------------------------------------------------------------------------

with Interfaces;                        use Interfaces;
with Ada.Numerics;                      use Ada.Numerics;
with Graphics.Frame_Buffer;
with Graphics.Modes;

package body Screen.Effects is
   use Graphics;

   procedure Gouraud_Hor_Line (
     X1, X2 : in     Integer;
     Y      : in     Integer;
     C1, C2 : in     Colors.Color_Index
   ) is

      Span: Integer:= X2-X1;
      fixColor, DeltaCol: sfix;
      Index: integer;
      Buffer_Data: Graphics.Frame_Buffer.Frame_Buffer_Pointer
        := Graphics.Frame_Buffer.Get_Frame_Buffer;

   begin
      if Span > 0 then

         fixColor:= sfix(C1) * sfix_one;
         Index:= X1 + Y * Graphics.Modes.Get_Width;
         DeltaCol:= ((sfix(C2)-sfix(C1)) * sfix_one) / sfix(Span);

         declare
            Cut: Integer; -- How much to cut at ends
         begin
            Cut:= XLeftClip  - X1;
            if Cut > 0 then -- Clip at end #1
               Span:= Span - Cut;
               Index:= Index + Cut;
               fixColor:= fixColor + DeltaCol * sfix(Cut);
            end if;
            Cut:= X2 - XRightClip;
            if Cut > 0 then -- Clip at end #2
               Span:= Span - Cut;
            end if;
         end;
         -- Clipping now prepared --

         for I in reverse 0 .. Span loop
            Buffer_Data(Index):= Colors.Color_Index (fixColor / sfix_one);
            Index:= Index + 1;
            fixColor:= fixColor + DeltaCol;
         end loop;
      end if;
   end Gouraud_Hor_Line;

  Texture_Data: Data_Buffer_Access;
  Texture_x_sfix_mask:  usfix;
  Texture_y_sfix_mask:  usfix;
  sfx_rep_U, sfx_rep_V: usfix; -- aff.
  fl_sfix_rep_U, fl_sfix_rep_V:   Real; -- persp.

  procedure Set_Current_texture
     (Texture      : in Texture_map;
      rep_U, rep_V : in Positive ) is
    begin
      Texture_Data:= Texture.Data;
      Texture_x_sfix_mask:= Texture.x_sfix_mask;
      Texture_y_sfix_mask:= Texture.y_sfix_mask;
      sfx_rep_U:= usfix(rep_U);
      sfx_rep_V:= usfix(rep_V * Texture.width);
      fl_sfix_rep_U:= fl_sfix_one * Real(rep_U);
      fl_sfix_rep_V:= fl_sfix_one * Real(rep_V * Texture.width);
    end;
    
  intensity_map: array(Colors.Color_Index, Intensity ) of Colors.Color_Index;
  -- NB: for RGB "true-color" it might be a good idea to have
  -- a function instead of an array, or to round colour value...

--  procedure Affine_TextureMap_Hor_Line
--     (Buffer         : in out Screen_Buffer;
--      X1, X2         : in     Integer;
--      Y              : in     Integer;
--      U1, V1, U2, V2 : in     Sfix;
--      int1, int2     : in     Intensity) is
  procedure Affine_TextureMap_Hor_Line
     (X1, X2         : in     Integer;
      Y              : in     Integer;
      U1, V1, U2, V2 : in     Sfix;
      int1, int2     : in     Intensity) is
      use type Colors.Color_Index;
    DeltaU, DeltaV, Uleft, Uright, Vleft, Vright: usfix;
    Index: Integer;
    Color: Colors.Color_Index;
    int:   Intensity;
    fix_int, Delta_int: sfix;
    variable_intensity: constant boolean:= int1 /= int2;
    Span: Integer:= X2-X1;
--    Buffer_Data:  Data_Buffer_Access:= Buffer.Data;
    Buffer_Data: Graphics.Frame_Buffer.Frame_Buffer_Pointer
      := Graphics.Frame_Buffer.Get_Frame_Buffer;

   begin
    if Span > 0 then

      Uleft :=  sfx_rep_U * usfix(U1);
      Uright:=  sfx_rep_U * usfix(U2);
      Vleft :=  sfx_rep_V * usfix(V1);
      Vright:=  sfx_rep_V * usfix(V2);
      fix_int:= sfix(int1) * sfix_one;

--      Index:= X1 + Y * Buffer.Width;
      Index:= X1 + Y * Graphics.Modes.Get_Width;
      DeltaU :=   usfix((sfix(Uright)-sfix(Uleft)) / sfix(Span));
      DeltaV :=   usfix((sfix(Vright)-sfix(Vleft)) / sfix(Span));
      Delta_int:= ((sfix(int2)-sfix(int1)) * sfix_one) / sfix(Span);

      -- Clipping --
      declare Cut: Integer; -- How much to cut at ends
      begin
        Cut:= XLeftClip  - X1;
        if Cut > 0 then -- Clip at end #1
          Span:= Span - Cut;
          Index:= Index + Cut;
          Uleft:= Uleft + DeltaU * usfix(Cut);
          Vleft:= Vleft + DeltaV * usfix(Cut);
          fix_int:= fix_int + Delta_int * sfix(Cut);
        end if;
        Cut:= X2 - XRightClip;
        if Cut > 0 then -- Clip at end #2
          Span:= Span - Cut;
        end if;
      end;
      -- Clipping now prepared --

      if variable_intensity then -- Gouraud effect

         for I in reverse 0 .. Span loop
--           color:= Texture_Data(Natural(
--                          (Uleft and Texture_x_sfix_mask) or
--                          (Vleft and Texture_y_sfix_mask)
--                          ) / sfix_one );
           color:= Texture_Data(Natural(
                          (Uleft and Texture_x_sfix_mask) or
                          (Vleft and Texture_y_sfix_mask)
                          ) / sfix_one );
           if color /= transparency_color then
             int:= Intensity (fix_int / sfix_one);
             if int/=0 then
               color:= intensity_map(color,int);
             end if;
--             Buffer_Data(Index):= color;
             Buffer_Data(Index):= Colors.Color_Index (color);
           end if;
  
           Index:= Index + 1;
           Uleft:= Uleft + DeltaU;
           Vleft:= Vleft + DeltaV;
           fix_int:= fix_int + Delta_int;
         end loop;

       elsif int1/=0 then -- constant but non-neutral intensity:
         -- !! Copy/Paste from variable intensity part (apart of intensity!)

         for I in reverse 0 .. Span loop
--           color:= Texture_Data(Natural(
--                          (Uleft and Texture_x_sfix_mask) or
--                          (Vleft and Texture_y_sfix_mask)
--                          ) / sfix_one );
           color:= Texture_Data(Natural(
                          (Uleft and Texture_x_sfix_mask) or
                          (Vleft and Texture_y_sfix_mask)
                          ) / sfix_one );
           if color /= transparency_color then
--             Buffer_Data(Index):= intensity_map(color,int1);
             Buffer_Data(Index):= Colors.Color_Index (intensity_map(color,int1));
           end if;

           Index:= Index + 1;
           Uleft:= Uleft + DeltaU;
           Vleft:= Vleft + DeltaV;
         end loop;

      else -- constant neutral intensity -> raw colour
         -- !! Copy/Paste from variable intensity part (apart of intensity!)

         for I in reverse 0 .. Span loop
--           color:= Texture_Data(Natural(
--                          (Uleft and Texture_x_sfix_mask) or
--                          (Vleft and Texture_y_sfix_mask)
--                          ) / sfix_one );
           color:= Texture_Data(Natural(
                          (Uleft and Texture_x_sfix_mask) or
                          (Vleft and Texture_y_sfix_mask)
                          ) / sfix_one );
           if color /= transparency_color then
--             Buffer_Data(Index):= color;
             Buffer_Data(Index):= Colors.Color_Index (color);
           end if;

           Index:= Index + 1;
           Uleft:= Uleft + DeltaU;
           Vleft:= Vleft + DeltaV;
         end loop;
      end if; -- intensity cases
    end if;

   end Affine_TextureMap_Hor_Line;

--  procedure Affine_TextureMap_Ver_Line
--     (Buffer         : in out Screen_Buffer;
--      X              : in     Integer;
--      Y1, Y2         : in     Integer;
--      U1, V1, U2, V2 : in     sfix;
--      int1, int2     : in     Intensity) is
  procedure Affine_TextureMap_Ver_Line
     (X              : in     Integer;
      Y1, Y2         : in     Integer;
      U1, V1, U2, V2 : in     sfix;
      int1, int2     : in     Intensity) is
      use type Colors.Color_Index;
    DeltaU, DeltaV, Utop, Ubottom, Vtop, Vbottom: usfix;
    Index: integer;
    Color: Colors.Color_Index;
    int: Intensity;
    fix_int, Delta_int: sfix;
    variable_intensity: constant boolean:= int1 /= int2;
    Span: Integer:= Y2-Y1;
--    Buffer_Data:  Data_Buffer_Access:= Buffer.Data;
    Buffer_Data: Graphics.Frame_Buffer.Frame_Buffer_Pointer
      := Graphics.Frame_Buffer.Get_Frame_Buffer;
--    Buffer_Width: Integer:= Buffer.Width;
    Buffer_Width: Integer:= Graphics.Modes.Get_Width;

   begin
    if Span > 0 then

      Utop :=   sfx_rep_U  * usfix(U1);
      Ubottom:= sfx_rep_U  * usfix(U2);
      Vtop :=   sfx_rep_V  * usfix(V1);
      Vbottom:= sfx_rep_V  * usfix(V2);
      fix_int:= sfix(int1) * sfix_one;

      Index:= X + Y1 * Buffer_Width;
      DeltaU := usfix((sfix(Ubottom)-sfix(Utop)) / sfix(Span));
      DeltaV := usfix((sfix(Vbottom)-sfix(Vtop)) / sfix(Span));
      Delta_int:= ((sfix(int2)-sfix(int1)) * sfix_one) / sfix(Span);

      -- Clipping --
      declare Cut: Integer; -- How much to cut at ends
      begin
        Cut:= YTopClip  - Y1;
        if Cut > 0 then -- Clip at end #1
          Span:= Span - Cut;
          Index:= Index + Cut * Buffer_Width;
          Utop:= Utop + DeltaU * usfix(Cut);
          Vtop:= Vtop + DeltaV * usfix(Cut);
          fix_int:= fix_int + Delta_int * sfix(Cut);
        end if;
        Cut:= Y2 - YBotClip;
        if Cut > 0 then -- Clip at end #2
          Span:= Span - Cut;
        end if;
      end;
      -- Clipping now prepared --

      if variable_intensity then -- Gouraud effect

        for I in reverse 0 .. Span loop
--          color:= Texture_Data(Natural(
--                         (Utop and Texture_x_sfix_mask) or
--                         (Vtop and Texture_y_sfix_mask)
--                         ) / sfix_one );
          color:= Texture_Data(Natural(
                         (Utop and Texture_x_sfix_mask) or
                         (Vtop and Texture_y_sfix_mask)
                         ) / sfix_one );
          if color /= transparency_color then
            int:= Intensity (fix_int / sfix_one);
            if int/=0 then
              color:= intensity_map(color,int);
            end if;
--            Buffer_Data(Index):= color;
            Buffer_Data(Index):= Colors.Color_Index (color);
          end if;
  
          Index:= Index + Buffer_Width;
          Utop:= Utop + DeltaU;
          Vtop:= Vtop + DeltaV;
          fix_int:= fix_int + Delta_int;
        end loop;

      elsif int1/=0 then -- constant but non-neutral intensity:
         -- !! Copy/Paste from variable intensity part (apart of intensity!)

         for I in reverse 0 .. Span loop
--           color:= Texture_Data(Natural(
--                          (Utop and Texture_x_sfix_mask) or
--                          (Vtop and Texture_y_sfix_mask)
--                          ) / sfix_one );
           color:= Texture_Data(Natural(
                          (Utop and Texture_x_sfix_mask) or
                          (Vtop and Texture_y_sfix_mask)
                          ) / sfix_one );
           if color /= transparency_color then
--             Buffer_Data(Index):= intensity_map(color,int1);
             Buffer_Data(Index):= Colors.Color_Index (intensity_map(color,int1));
           end if;

           Index:= Index + Buffer_Width;
           Utop:= Utop + DeltaU;
           Vtop:= Vtop + DeltaV;
         end loop;

      else -- constant neutral intensity -> raw colour
       -- !! Copy/Paste from variable intensity part (apart of intensity!)

         for I in reverse 0 .. Span loop
--           color:= Texture_Data(Natural(
--                          (Utop and Texture_x_sfix_mask) or
--                          (Vtop and Texture_y_sfix_mask)
--                          ) / sfix_one );
           color:= Texture_Data(Natural(
                          (Utop and Texture_x_sfix_mask) or
                          (Vtop and Texture_y_sfix_mask)
                          ) / sfix_one );
           if color /= transparency_color then
--             Buffer_Data(Index):= color;
             Buffer_Data(Index):= Colors.Color_Index (color);
           end if;

           Index:= Index + Buffer_Width;
           Utop:= Utop + DeltaU;
           Vtop:= Vtop + DeltaV;
         end loop;
      end if; -- intensity cases
    end if;

   end Affine_TextureMap_Ver_Line;

--  procedure TextureMap_Hor_Line
--     (Buffer             : in out Screen_Buffer;
--      X1, X2             : in     Integer;
--      Y                  : in     Integer;
--      UP1, VP1, UP2, VP2 : in     Real;
--      iv_Z1, iv_Z2       : in     Real;
--      int1, int2         : in     Intensity) is
  procedure TextureMap_Hor_Line
     (X1, X2             : in     Integer;
      Y                  : in     Integer;
      UP1, VP1, UP2, VP2 : in     Real;
      iv_Z1, iv_Z2       : in     Real;
      int1, int2         : in     Intensity) is
      use type Colors.Color_Index;
    -- In this local model, the focal = 1 (not importing), eye at Z=0

    Span: Integer:= X2-X1;
    UP_sfx_right, VP_sfx_right, UP_sfx, VP_sfx,
    Delta_UP_sfx, Delta_VP_sfx,
    Delta_UP_sfx_granu, Delta_VP_sfx_granu,
    Z, iv_Z, Delta_iv_Z, Delta_iv_Z_granu: Real;
    Index: integer;
    U, V: usfix;
    U_new, V_new: sfix;  -- signed because of possible <0 values
    DeltaU, DeltaV : usfix;
    Color: Colors.Color_Index;
    variable_intensity: constant boolean:= int1 /= int2;
    int: Intensity;
    fix_int, Delta_int: sfix;
--    Buffer_Data:  Data_Buffer_Access:= Buffer.Data;
    Buffer_Data: Graphics.Frame_Buffer.Frame_Buffer_Pointer
      := Graphics.Frame_Buffer.Get_Frame_Buffer;

   -- Every "granu_persp" pixels, we calculate correct Z, U=Z*UP, V=Z*VP

   -- values:       1 : correct perspective, but it means 1 FPU div per pixel!
   --              >8 : Pentium-friendly (CPU/FPU concurrency)
   --      "infinity" : affine - "swimmy" textures :-(

   granu_persp: constant:= 32;   -- 2 ** something please...
   r_granu_persp: constant Real:= Real(granu_persp);

  -- We pre-increment 1/Z to avoid using Z too short after
  -- the division (CPU // FPU strategy on pentium+)
   procedure Pre_inc_Z(Span_minus_J: Integer) is
     begin
      if Span_minus_J >= granu_persp then
        iv_Z:= iv_Z + Delta_iv_Z_granu;
      else
        iv_Z:= iv_Z + Real(Span_minus_J) * Delta_iv_Z;
      end if;
      Z:= 1.0 / iv_Z;  -- <- The mighty division we try to avoid at best
     end;
   pragma Inline(Pre_Inc_Z);

   begin
    if Span > 0 then

      UP_sfx :=      UP1 * fl_sfix_rep_U;
      UP_sfx_right:= UP2 * fl_sfix_rep_U;
      VP_sfx :=      VP1 * fl_sfix_rep_V;
      VP_sfx_right:= VP2 * fl_sfix_rep_V;
      iv_Z:=         iv_Z1;
      fix_int:=      sfix(int1) * sfix_one;

--      Index:= X1 + Y * Buffer.Width;
      Index:= X1 + Y * Graphics.Modes.Get_Width;
      declare delta_pixel: constant Real:= 1.0 / Real(span);
      begin
        Delta_UP_sfx:=  delta_pixel * (UP_sfx_right - UP_sfx);
        Delta_UP_sfx_granu:=  r_granu_persp * Delta_UP_sfx;
        Delta_VP_sfx:=  delta_pixel * (VP_sfx_right - VP_sfx);
        Delta_VP_sfx_granu:=  r_granu_persp * Delta_VP_sfx;
        Delta_iv_Z:=    delta_pixel * (iv_Z2 - iv_Z);
        Delta_iv_Z_granu:= r_granu_persp * Delta_iv_Z;
      end;
      Delta_int:= ((sfix(int2)-sfix(int1)) * sfix_one) / sfix(Span);

      -- Clipping --
      declare Cut: Integer; RCut: Real; -- How much to cut at ends
      begin
        Cut:= XLeftClip  - X1;
        if Cut > 0 then -- Clip at end #1
          Span:= Span - Cut;
          Index:= Index + Cut;
          RCut:= Real(Cut);
          UP_sfx:=  UP_sfx  + Delta_UP_sfx * RCut;
          VP_sfx:=  VP_sfx  + Delta_VP_sfx * RCut;
          iv_Z:=    iv_Z    + Delta_iv_Z   * RCut;
          fix_int:= fix_int + Delta_int    * sfix(Cut);
        end if;

        Z:= 1.0 / iv_Z;

        Cut:= X2 - XRightClip;
        if Cut > 0 then -- Clip at end #2
          Span:= Span - Cut;
          RCut:= Real(Cut);
          UP_sfx_right:=  UP_sfx_right  - Delta_UP_sfx * RCut;
          VP_sfx_right:=  VP_sfx_right  - Delta_VP_sfx * RCut;
        end if;
      end;
      -- Clipping now prepared --

      -- The trick is that 1/Z, UP=U/Z, VP=V/Z are affine functions of
      -- (XP,YP) = (X/Z, Y/Z), the projected (P) screen coordinates

      U_new:= sfix( UP_sfx * Z );
      V_new:= sfix( VP_sfx * Z );

      Pre_inc_Z(Span);

      if variable_intensity then -- Gouraud effect

           for Span_minus_I in reverse 0 .. Span loop

             if (Span - Span_minus_I) mod granu_persp = 0 then

               -- It's time to calculate the next perspectively correct U,V
               -- that we will interpolate linearily between

               -- Keep the "old new" as incremented bit-maskable fixed-point coor.
               U:= usfix(U_new);
               V:= usfix(V_new);
               if Span_minus_I > granu_persp then
                 UP_sfx:= UP_sfx + Delta_UP_sfx_granu;
                 VP_sfx:= VP_sfx + Delta_VP_sfx_granu;
                 U_new:= sfix( UP_sfx * Z );
                 V_new:= sfix( VP_sfx * Z );
                 -- Calculate the deltas for non-projected things:
                 DeltaU:= usfix(( U_new-sfix(U) ) / granu_persp);
                 DeltaV:= usfix(( V_new-sfix(V) ) / granu_persp);
                 Pre_inc_Z( Span_minus_I - granu_persp );  -- Pre-increment 1/Z.
               elsif Span_minus_I > 0 then  -- Last granular bunch, with >= 1 pixel:
                 U_new:= sfix( UP_sfx_right * Z );
                 V_new:= sfix( VP_sfx_right * Z );
                 -- Calculate the deltas for non-projected things:
                 DeltaU:= usfix(( U_new-sfix(U) ) / sfix(Span_minus_I));
                 DeltaV:= usfix(( V_new-sfix(V) ) / sfix(Span_minus_I));
               end if;   -- else: we don't care, it's the last pixel!

             else   -- the affine side of the algo...

               U:= U + DeltaU;
               V:= V + DeltaV;

             end if;

--             color:= Texture_Data(Natural(
--                              (U and Texture_x_sfix_mask) or
--                              (V and Texture_y_sfix_mask)
--                              ) / sfix_one );
             color:= Texture_Data(Natural(
                              (U and Texture_x_sfix_mask) or
                              (V and Texture_y_sfix_mask)
                              ) / sfix_one );

             if color /= transparency_color then
               int:= Intensity (fix_int / sfix_one);
               if int/=0 then
                 color:= intensity_map(color,int);
               end if;
--               Buffer_Data(Index):= color;
               Buffer_Data(Index):= Colors.Color_Index (color);
             end if;

             Index:= Index + 1;
             fix_int:= fix_int + Delta_int;
           end loop;

       elsif int1/=0 then -- constant but non-neutral intensity:
         -- !! Copy/Paste from variable intensity part (apart of intensity!)
         -- Already tried a version with generics - but it doesn't inline...

           for Span_minus_I in reverse 0 .. Span loop

             if (Span - Span_minus_I) mod granu_persp = 0 then

               -- It's time to calculate the next perspectively correct U,V
               -- that we will interpolate linearily between

               -- Keep the "old new" as incremented bit-maskable fixed-point coor.
               U:= usfix(U_new);
               V:= usfix(V_new);
               if Span_minus_I > granu_persp then
                 UP_sfx:= UP_sfx + Delta_UP_sfx_granu;
                 VP_sfx:= VP_sfx + Delta_VP_sfx_granu;
                 U_new:= sfix( UP_sfx * Z );
                 V_new:= sfix( VP_sfx * Z );
                 -- Calculate the deltas for non-projected things:
                 DeltaU:= usfix(( U_new-sfix(U) ) / granu_persp);
                 DeltaV:= usfix(( V_new-sfix(V) ) / granu_persp);
                 Pre_inc_Z( Span_minus_I - granu_persp);  -- Pre-increment 1/Z.
               elsif Span_minus_I > 0 then  -- Last granular bunch, with >= 1 pixel:
                 U_new:= sfix( UP_sfx_right * Z );
                 V_new:= sfix( VP_sfx_right * Z );
                 -- Calculate the deltas for non-projected things:
                 DeltaU:= usfix(( U_new-sfix(U) ) / sfix(Span_minus_I));
                 DeltaV:= usfix(( V_new-sfix(V) ) / sfix(Span_minus_I));
               end if;   -- else: we don't care, it's the last pixel!

             else   -- the affine side of the algo...

               U:= U + DeltaU;
               V:= V + DeltaV;

             end if;

--             color:= Texture_Data(Natural(
--                              (U and Texture_x_sfix_mask) or
--                              (V and Texture_y_sfix_mask)
--                              ) / sfix_one );
             color:= Texture_Data(Natural(
                              (U and Texture_x_sfix_mask) or
                              (V and Texture_y_sfix_mask)
                              ) / sfix_one );

             if color /= transparency_color then
--               Buffer_Data(Index):= intensity_map(color,int1);
               Buffer_Data(Index):= Colors.Color_Index (intensity_map(color,int1));
             end if;

             Index:= Index + 1;
           end loop;

      else -- constant neutral intensity -> raw colour
       -- !! Copy/Paste from variable intensity part (apart of intensity!)

           for Span_minus_I in reverse 0 .. Span loop

             if (Span - Span_minus_I) mod granu_persp = 0 then

               -- It's time to calculate the next perspectively correct U,V
               -- that we will interpolate linearily between

               -- Keep the "old new" as incremented bit-maskable fixed-point coor.
               U:= usfix(U_new);
               V:= usfix(V_new);
               if Span_minus_I > granu_persp then
                 UP_sfx:= UP_sfx + Delta_UP_sfx_granu;
                 VP_sfx:= VP_sfx + Delta_VP_sfx_granu;
                 U_new:= sfix( UP_sfx * Z );
                 V_new:= sfix( VP_sfx * Z );
                 -- Calculate the deltas for non-projected things:
                 DeltaU:= usfix(( U_new-sfix(U) ) / granu_persp);
                 DeltaV:= usfix(( V_new-sfix(V) ) / granu_persp);
                 Pre_inc_Z( Span_minus_I - granu_persp);  -- Pre-increment 1/Z.
               elsif Span_minus_I > 0 then  -- Last granular bunch, with >= 1 pixel:
                 U_new:= sfix( UP_sfx_right * Z );
                 V_new:= sfix( VP_sfx_right * Z );
                 -- Calculate the deltas for non-projected things:
                 DeltaU:= usfix(( U_new-sfix(U) ) / sfix(Span_minus_I));
                 DeltaV:= usfix(( V_new-sfix(V) ) / sfix(Span_minus_I));
               end if;   -- else: we don't care, it's the last pixel!

             else   -- the affine side of the algo...

               U:= U + DeltaU;
               V:= V + DeltaV;

             end if;

--             color:= Texture_Data(Natural(
--                              (U and Texture_x_sfix_mask) or
--                              (V and Texture_y_sfix_mask)
--                              ) / sfix_one );
             color:= Texture_Data(Natural(
                              (U and Texture_x_sfix_mask) or
                              (V and Texture_y_sfix_mask)
                              ) / sfix_one );

             if color /= transparency_color then
--               Buffer_Data(Index):= color;
               Buffer_Data(Index):= Colors.Color_Index (color);
             end if;

             Index:= Index + 1;
           end loop;

      end if; -- intensity cases
    end if;

   end TextureMap_Hor_Line;

  Phong_vals: constant:= 64;
  subtype Phong_val is Natural range 0..Phong_vals-1;
  type Phong_map is array(Phong_index, Phong_index) of Phong_val;
  the_Phong_Map: Phong_map;

  procedure Calc_Phong_map(map: out Phong_map) is
    f: constant Real:= Real(Phong_val'last-1);
    p: constant:= 1.6;
    begin
      for i in map'range(1) loop
        for j in map'range(2) loop
          map(i,j):=
            Phong_val( (Sin(Real(i)*pi/Real(map'length(1))) ** p) *
                       (Sin(Real(j)*pi/Real(map'length(2))) ** p) * f)+1;
        end loop;
      end loop;
    end Calc_Phong_map;

--  procedure EnvMap_Hor_Line
--     (Buffer              : in out Screen_Buffer;
--      X1, X2              : in     Integer;
--      Y                   : in     Integer;
--      U1, V1, U2, V2      : in     Phong_index;
--      Cmin, Num_of_shades : in     Color_Index) is
  procedure EnvMap_Hor_Line
     (X1, X2         : in     Integer;
      Y              : in     Integer;
      U1, V1, U2, V2 : in     Phong_index;
      Cmin           : in     Colors.Color_Index;
      Num_of_shades  : in     Byte) is
     use type Colors.Color_Index;

    Span: Integer:= X2-X1;

    DeltaU, DeltaV : sfix;
    Uposfixed, Vposfixed: sfix;
    Index: integer;
    ph:  Phong_val;
--    Buffer_Data:  Data_Buffer_Access:= Buffer.Data;
    Buffer_Data: Graphics.Frame_Buffer.Frame_Buffer_Pointer
      := Graphics.Frame_Buffer.Get_Frame_Buffer;
   begin
    if Span > 0 then

--      Index:= X1 + Y * Buffer.Width;
      Index:= X1 + Y * Graphics.Modes.Get_Width;
      Uposfixed:= sfix(U1) * sfix_one;
      Vposfixed:= sfix(V1) * sfix_one;
      DeltaU := (sfix(U2-U1) * sfix_one) / sfix(Span);
      DeltaV := (sfix(V2-V1) * sfix_one) / sfix(Span);

      -- Clipping --
      declare Cut: Integer; -- How much to cut at ends
      begin
        Cut:= XLeftClip  - X1;
        if Cut > 0 then -- Clip at end #1
          Span:= Span - Cut;
          Index:= Index + Cut;
          Uposfixed:= Uposfixed + DeltaU * sfix(Cut);
          Vposfixed:= Vposfixed + DeltaV * sfix(Cut);
        end if;
        Cut:= X2 - XRightClip;
        if Cut > 0 then -- Clip at end #2
          Span:= Span - Cut;
        end if;
      end;
      -- Clipping now prepared --

      for I in reverse 0 .. Span loop
        ph:= the_Phong_map( Natural(Uposfixed / sfix_one),
                            Natural(Vposfixed / sfix_one) );
--        Buffer_Data(Index):= Cmin +
--         Color_Index( (Natural(Num_of_shades) * Natural(ph)) / Phong_vals );
        Buffer_Data(Index):= Cmin +
         Colors.Color_Index ( (Natural(Num_of_shades) * Natural(ph)) / Phong_vals );

        Index:= Index + 1;
        Uposfixed:= Uposfixed + DeltaU;
        Vposfixed:= Vposfixed + DeltaV;

      end loop;

    end if;
   end EnvMap_Hor_Line;

  procedure Reset_intensity_map is
    begin
      for c in Colors.Color_Index loop
        for i in Intensity loop
          intensity_map(c,i):= c; -- each colour is independant
        end loop;
      end loop;
    end Reset_intensity_map;

  procedure Calculate_intensity_map(d: degrade_description) is
     use type Colors.Color_Index;
    begin
      Reset_intensity_map;
      for i in d'range loop
        declare
          ca: Colors.Color_Index:= d(i).c;
          cb: Colors.Color_Index:= ca + Colors.Color_Index( abs(d(i).n) );
          fca: Real:= Real(ca);
          fcb: Real:= Real(cb);
          slope: Real:= - Real(Intensity'last) / Real( d(i).n );
          -- slope of an equi-colour level line
          ci, iv_slope: Real;
        begin
          if cb > ca then -- 1-colour d‚grad‚ is let resetted
            iv_slope:= 1.0 / slope;
            for c in ca .. cb loop
              for int in Intensity loop
                -- we use the fact that intensity_map(c,0) must be = c
               ci:= Real'floor( 0.5 + Real(c) - Real(int) * iv_slope );
               if ci >= fcb then ci:= fcb;
               elsif ci <= fca then ci:= fca; end if; -- ci outside the slide
               intensity_map(c,int):= Colors.Color_Index(ci);
              end loop;
            end loop;
          end if;
        end;
      end loop;
    end Calculate_intensity_map;

  procedure Set_Pixel(t: in out Texture_map; x,y: natural; Color: Colors.Color_Index) is
    begin
      t.Data( x + y * t.width ):= Colors.Color_Index (Color);
    end;

--  procedure Put_Buffer (Source      : in     Screen_Buffer;
--                        Destination : in out Texture_map) is
  procedure Put_Buffer (Destination : in out Texture_map) is
    begin
--      if Source.width  /= Destination.width  or else
--         Source.height /= Destination.height then
      if Graphics.Modes.Get_Width  /= Destination.width  or else
         Graphics.Modes.Get_Height /= Destination.height then
--        Raise Out_of_range;
        Raise Graphics.Graphics_Error;
      end if;
--      Destination.data.all:= Source.data.all;
      Destination.data.all:= Data_Buffer (
        Graphics.Frame_Buffer.Get_Frame_Buffer (Destination.data'Range)
      );
    end Put_Buffer;

   procedure Initialize (TM : in out Texture_map) is
--      Oversized_texture : exception;
      Size : Natural;
   begin
      if TM.x_bits + TM.y_bits + Sfix_bits  > 30 then
--        raise Oversized_texture;
         raise Graphics.Graphics_Error;
      end if;
      TM.width:=   2**TM.x_bits;
      TM.height:=  2**TM.y_bits;
      TM.x_mask:=  2**TM.x_bits -1;
      TM.y_mask:= (2**TM.y_bits -1) * (2**TM.x_bits);
      TM.x_sfix_mask:= TM.x_mask * sfix_one;
      TM.y_sfix_mask:= TM.y_mask * sfix_one;
      if TM.Data /= null then
--         raise Screen_Buffer_Error;
         raise Graphics.Graphics_Error;
      end if;
      Size := TM.Width * TM.Height;
      TM.Data := New Data_Buffer(0 .. Size - 1);
   end Initialize;

   procedure Finalize (TM : in out Texture_map) is
   begin
      Free (TM.Data);
      TM.Data := null;
   end Finalize;

begin
  Reset_intensity_map;
  Calc_Phong_map(the_Phong_map);
end Screen.Effects;
