------------------------------------------------------------------------------
--  File:            Demo3D01.adb
--  Description:     Demo/test for Engine_3D package - Portals
--  Date / Version:  8-Feb-2000
--  Author:          Gautier.deMontmollin@Maths.UniNe.CH
------------------------------------------------------------------------------

with Screen;
with Screen.IO;                         use Screen.IO;
with Screen.Effects;                    use Screen.Effects;
with Screen.Effects.IO;                 use Screen.Effects.IO;
with Game_Colors;                       use Game_Colors;
--with Game_Driving;                      use Game_Driving;

with Graphics.Colors;
with Graphics.Drawing;
with Graphics.Modes;

with Engine_3D;                         use Engine_3D;
with Engine_3D.Math;                    use Engine_3D.Math;
with Engine_3D.Merging;                 use Engine_3D.Merging;
with Engine_3D.Morphing;                use Engine_3D.Morphing;
with Engine_3D.Construct;               use Engine_3D.Construct;

with Icosaedron, Vehic001, X29, Shuttle3;

--with Multi_keys;                        use Multi_keys;
--with PC_Mouse;                          use PC_Mouse;
with Timer;
with Keyboard;
with Mouse;

with Unzip;                             use Unzip;
with Unzip.Streams;                     use Unzip.Streams;

with Min_Max;

with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Calendar;                      use Ada.Calendar;
with Ada.Numerics;                      use Ada.Numerics;
with Ada.Numerics.Elementary_functions; use Ada.Numerics.Elementary_functions;

procedure Demo_3D_01 is
  Spare_pal : Graphics.Colors.Palette_Type (Graphics.Colors.Color_Index);

  package IIO is new Integer_IO(integer);  use IIO;
  package FIO is new Float_IO(float);      use FIO;

  subtype Real is Engine_3D.Real; -- (and not SVGA.Effects.Real)
  package RIO is new Float_IO(Real);       use RIO;
  package MMR is new Min_Max(Real);        use MMR;

  Mouse_Installed : Boolean;

   type Command_Type is (
     Go_Backward,
     Go_Forward,
     Swing_Left,
     Swing_Right,
     Take_Snapshot,
     Turn_Down,
     Turn_Left,
     Turn_Right,
     Turn_Up,
     Quit
   );

   Key : array (Command_Type) of Keyboard.Key_Code := (
     Go_Backward   => Keyboard.Down,
     Go_Forward    => Keyboard.Up,
     Swing_Left    => Keyboard.S,
     Swing_Right   => Keyboard.D,
     Take_Snapshot => Keyboard.X,
     Turn_Down     => Keyboard.Z,
     Turn_Left     => Keyboard.Left,
     Turn_Right    => Keyboard.Right,
     Turn_Up       => Keyboard.A,
     Quit          => Keyboard.Escape
   );

   function Commanded (Command : Command_Type) return Boolean is
   begin
      return Keyboard.Is_Pressed (Key (Command));
   end Commanded;

   function Do_Once (Command : Command_Type) return Boolean is
   begin
      return Keyboard.Was_Struck (Key (Command));
   end Do_Once;

  zip: constant string:= "demo3d01.dat";
  zif: Unzip.zip_info; -- zip directory structure for fast access

  procedure Load_zipped_BMP_texture(name: String; Texture: out p_Texture_map) is
    f: Zipped_File_Type;
    begin
      Open(f,zif,name);
      Load_BMP_texture( Stream(f), Texture );
      Close(f);
    end Load_zipped_BMP_texture;

  procedure Load_zipped_BMP_palette(name: String; Palette:  out Graphics.Colors.Palette_Type) is
    f: Zipped_File_Type;
    width: Integer; height: Integer;
    begin
      Open(f,zif,name);
      Read_BMP_Header( Stream(f), width, height);
      Load_BMP_palette( Stream(f), Palette );
      Close(f);
    end Load_zipped_BMP_palette;

  type texture_id is
  ( Beton001, BoisCrac, CadrBrul, CroiCroi,
    FentJaun, Gobelin2, Grilles,  PierBleu,
    Rouille1, Rouille2, Rouille3, Rouille4,
    VerOpakG, Vitre );

  tex: array(texture_id) of p_Texture_map;

  ----------
  -- laby --
  ----------

  laby: a_p_Object_3D(1..3);
  t: integer:= 2*screen_virtual_size;

  procedure Make_Room( r: in out Object_3D;
                       tex_wall: texture_id;
                       tw_rep: Natural;
                       po1,po2,po3,po4: p_Object_3D;
                       ti1,ti2,ti3,ti4: texture_id) is

--   c1: constant Color_Type:= 1;
--   c2: constant Color_Type:= 30;
   c1: constant Graphics.Colors.Color_Index := 1;
   c2: constant Graphics.Colors.Color_Index := 30;
   ia: constant intensity:= intensity'first;
   ib: constant intensity:= intensity'last;
   ia_sol: constant intensity:= intensity'first;
   ib_sol: constant intensity:= (intensity'last*2)/3;
   seg: constant:= 4; -- pair!
   p1,p2,p3,p4: Point;
   por: p_Object_3D;
   tid: texture_id;

   subtype wall_nr is integer range 1..4;
   procedure Rotate_points(w:wall_nr) is
     begin
       case w is
         when 1 => por:= po1;
                   tid:= ti1;
         when 2 => p1:= (t-p1.y, p1.x,p1.z);
                   p2:= (t-p2.y, p2.x,p2.z);
                   p3:= (t-p3.y, p3.x,p3.z);
                   p4:= (t-p4.y, p4.x,p4.z);
                   por:= po2;
                   tid:= ti2;
         when 3 => p1:= (t-p1.x,t-p1.y,p1.z);
                   p2:= (t-p2.x,t-p2.y,p2.z);
                   p3:= (t-p3.x,t-p3.y,p3.z);
                   p4:= (t-p4.x,t-p4.y,p4.z);
                   por:= po3;
                   tid:= ti3;
         when 4 => p1:= ( p1.y,t-p1.x,p1.z);
                   p2:= ( p2.y,t-p2.x,p2.z);
                   p3:= ( p3.y,t-p3.x,p3.z);
                   p4:= ( p4.y,t-p4.x,p4.z);
                   por:= po4;
                   tid:= ti4;
       end case;
     end Rotate_points;

   begin
     r.Num_of_points:= 0;
     r.Num_of_faces := 0;
     
     for v in -seg .. seg loop
       for u in -seg .. seg loop
         Create_face(
           ((0,0,0,0), True, tex(Beton001), 1,1, ia_sol,ib_sol, c1,c1),
           ( (u*t,v*t,0), ((u+1)*t,v*t,0),
             ((u+1)*t,(v+1)*t,0), (u*t,(v+1)*t,0) ),
           ( True, True, True, True ),
           r ); -- sol
       end loop;
     end loop;

     for v in -seg .. seg loop
       for u in -seg .. seg loop
         Create_face(
           ((0,0,0,0), False, tex(BoisCrac), 1,1, ia,ib/3, 64,81),
           ( (u*t,v*t,3*t), (u*t,(v+1)*t,3*t),
             ((u+1)*t,(v+1)*t,3*t), ((u+1)*t,v*t,3*t) ),
           ( True, True, True, True ),
           r ); -- plafond
       end loop;
     end loop;

     for mur in 1..4 loop
       p1:= (-t,(seg+1)*t,0);
       p2:= (0,(seg+1)*t,0);
       p3:= (0,(seg+2)*t,0);
       Rotate_points(mur);
       Create_face(
         ((0,0,0,0), True, tex(Beton001), 1,1, ia_sol,ib_sol, c1,c1),
         ( p1,p2,p3,p4 ), ( True, True, True, False ),
         r ); -- seuil g.
       p1:= ( t,(seg+1)*t,0);
       p2:= ( 2*t,(seg+1)*t,0);
       p4:= ( t,(seg+2)*t,0);
       Rotate_points(mur);
       Create_face(
         ((0,0,0,0), True, tex(Beton001), 1,1, ia_sol,ib_sol, c1,c1),
         ( p1,p2,p3,p4 ), ( True, True, False, True ),
         r ); -- seuil d.
       p1:= ( 0,(seg+1)*t,0);
       p2:= ( t,(seg+1)*t,0);
       p3:= ( t,(seg+2)*t,0);
       p4:= ( 0,(seg+2)*t,0);
       Rotate_points(mur);
       Create_face(
         ((0,0,0,0), True, tex(Beton001), 1,1, ia_sol,ib_sol, c1,c1),
         ( p1,p2,p3,p4 ), ( True, True, True, True ),
         r ); -- seuil c.

       p1:= ( 0,(seg+1)*t,t);
       p2:= ( 0,(seg+2)*t,t);
       p4:= ( 0,(seg+1)*t,2*t);
       Rotate_points(mur);
       Create_face(
         ((0,0,0,0), True, tex(tex_wall), tw_rep, tw_rep, ia,ib, c1,c2),
          (p1,p2,p3,p4), ( True, True, False, True ),
         r ); -- bout de mur g.

       p2:= ( 0,(seg+2)*t,t);
       p3:= ( 0,(seg+1)*t,t);
       p4:= (-t,(seg+1)*t,t);
       Rotate_points(mur);
       Create_face(
         ((0,0,0,0), True, tex(tex_wall), tw_rep, tw_rep, ia,ib, c1,c2),
          (p1,p2,p3,p4), ( False, True, True, True ),
         r ); -- bout de mur sous, g.

       p1:= ( t,(seg+2)*t,t);
       p2:= ( t,(seg+1)*t,t);
       p3:= ( t,(seg+1)*t,2*t);
       Rotate_points(mur);
       Create_face(
         ((0,0,0,0), True, tex(tex_wall), tw_rep, tw_rep, ia,ib, c1,c2),
          (p1,p2,p3,p4), ( True, True, True, False ),
         r ); -- bout de mur d.

       p1:= (  t,(seg+2)*t,t);
       p3:= (2*t,(seg+1)*t,t);
       p4:= (  t,(seg+1)*t,t);
       Rotate_points(mur);
       Create_face(
         ((0,0,0,0), True, tex(tex_wall), tw_rep, tw_rep, ia,ib, c1,c2),
          (p1,p2,p3,p4), ( True, False, True, True ),
         r ); -- bout de mur sous, d.

       for u in -seg .. seg loop
         for v in 0..2 loop
           p1:= (u*t,(seg+1)*t,v*t);
           p2:= ((u+1)*t,(seg+1)*t,v*t);
           p3:= ((u+1)*t,(seg+1)*t,(v+1)*t);
           p4:= (u*t,(seg+1)*t,(v+1)*t);
           if u=0 and then v=0 then
             p1.y:= p1.y+t;
             p2.y:= p2.y+t;
             p3.y:= p3.y+t;
             p4.y:= p4.y+t;
           elsif u=-1 and then v=0 then
             p2.y:= p2.y+t;
             p3.y:= p3.y+t;
           elsif u=+1 and then v=0 then
             p1.y:= p1.y+t;
             p4.y:= p4.y+t;
           elsif u=0 and then v=1 then
             p1.y:= p1.y+t;
             p2.y:= p2.y+t;
           end if;
           Rotate_points(mur);
           Create_face(
             ((0,0,0,0), True, tex(tex_wall), tw_rep, tw_rep, ia,ib, c1, c2),
              (p1,p2,p3,p4), ( True, True, True, True ),
             r ); -- murs
           if u=0 and then v=0 and then por /= Null then
             case tid is
               when Vitre => r.faces(r.Num_of_faces).rep_U:=6;
                             r.faces(r.Num_of_faces).rep_V:=6;
               when others=> null;
             end case;
             r.Portal(r.Num_of_faces):=  (see_through, false, por);
             r.faces(r.Num_of_faces).texture:= tex(tid);
           end if;
         end loop;
       end loop;
     end loop;

   end Make_Room;

  Procedure Init_laby is
   use Shuttle3, Vehic001;
   begin
     for lr in laby'range loop
       laby(lr):= New Object_3D( Max_points=> 1000, Max_faces=> 1000 );
     end loop;
     Make_Room( laby(3).all,
                rouille1, 2,
                null,null, null, null,
                Vitre, Vitre, Vitre, Vitre );

     vehicle_001.center:= (0.0,Real(t),0.0);
     vehicle_001.auto_rotation:=
        To_Fix(XYZ_rotation( 0.1, -0.02, -0.6 ));

     laby(3):= 1.0 * laby(3).all +
               ( 5.0 * vehicle_001.all +
                 Vector3'(0.0,0.0,0.5*Real(t)));
               -- Att: nouv. pointeur!
     laby(3).center:= (Real(-8*t),0.0,0.0);

     Make_Room( laby(2).all,
                rouille4, 4,
                null,null, null, null,
                Vitre, Vitre, Vitre, Vitre );
     Shuttle3_obj.center:= (0.0,0.0,0.0);
     Shuttle3_obj.auto_rotation:=
        To_Fix(XYZ_rotation( 0.1, -0.02, -1.0 ));

     laby(2):= 1.0 * laby(2).all +
               ( 7.5 * Shuttle3_obj.all +
                 Vector3'(0.0,0.0,0.5*Real(t)));
               -- Att: nouv. pointeur!
     laby(2).center:= (0.0,Real(8*t),0.0);

     Make_Room( laby(1).all,
                croicroi, 2,
                laby(2), laby(3), null, null,
                Vitre, Vitre, Vitre, Vitre );

     for lr in laby'range loop
       Put( laby(lr).id, lr );
       laby(lr).id(1..10):= "Labyrinthe";
       Init_object(laby(lr).all);
     end loop;
   end Init_laby;

 procedure Demo_3D_01_in_graphics is

  procedure Reset_lightings is
    begin
      parallel_lights:= 4;
      parallel_light_force(1):= 0.3;
      Find_light_direction( (+50,+10,-20),(0,0,0), parallel_light_vect(1) );
      parallel_light_force(2):= 0.3;
      Find_light_direction( (-50,+10,-20),(0,0,0), parallel_light_vect(2) );
      parallel_light_force(3):= 0.3;
      Find_light_direction( (+10,-50,-10),(0,0,0), parallel_light_vect(3) );
      parallel_light_force(4):= 0.4;
      Find_light_direction( (-10,+50,+10),(0,0,0), parallel_light_vect(4) );

      radial_lights:= 1;
      radial_light_force(1):= 4.0;
    end;

--  centrX, centrY: integer; -- Position souris … l'init.
  centrX, centrY: integer := 50; -- Position souris … l'init.

  -- Fine timing
  freq: constant:= 600.0; -- Hz

  -- This is an off-screen frame buffer
--  type Frame_Access is access Screen_Buffer;
--  Buffer : Frame_Access;    -- The buffer

  previously_left_button_pressed: boolean:= false;

  escape: boolean:= false;

  vect_dir: Vector3;
  vitesse: Real:= 0.0;
  dswing, dlater, dverti: Real:= 0.0;
  last_time: float:= 0.0;

-- Photo_BMP
  snaps: natural:= 0;
  procedure Photo_BMP is
    ext: String(1..4);
  begin
    snaps:= snaps + 1;
    Put(To=> ext, Item=> 1000 + snaps); 
    Graphics.Colors.Get_Palette (Spare_pal);
--    Save_BMP("snap_" & ext(2..4) & ".bmp", Buffer.all, Get_Palette);
    Save_BMP("snap_" & ext(2..4) & ".bmp", Spare_pal);
  end Photo_BMP;

  procedure Common_ops is
    elaps, fine_time: float;
    rota: constant:= 0.5*pi;
    pas, pas_angle, attenu: Real;
--    c: command_set:= no_command;
    gx, gy: Float;

    begin
--      Put_Buffer(Buffer.all);     --  Copy the buffer to the screen
       Graphics.Copy_Graphics_To_Screen;

--      Record_commands(c, centrX,centrY, Graphics.Modes.Get_Width, Graphics.Modes.Get_Height, gx, gy);

      Keyboard.Update;

--      if c(photo) then -- 'x' on US and other keyboards
      if Commanded (Take_Snapshot) then -- 'x' on US and other keyboards
        Photo_BMP;
      end if;

      Graphics.Drawing.Clear_Graphics (0);  --  Clear the buffer

      fine_time:= float(Timer.Clock);
      elaps:= fine_time - last_time;

      last_time:= fine_time;

      pas:= fl_screen_virtual_size * 1.0 * Real(elaps);
      pas_angle:= 0.1*pi*Real(elaps);
      attenu:= max(0.02,min(0.98,1.0 - Real(elaps)*3.0));

      if Commanded (Go_Forward)   then vitesse:= vitesse + pas; end if;
      if Commanded (Go_Backward) then vitesse:= vitesse - pas; end if;
--QQ      if c(go_graduated) then vitesse:= vitesse + Real(gy) * pas; end if;
      if Commanded (Turn_Left)  then dlater:= dlater - pas_angle; end if;
      if Commanded (Turn_Right) then dlater:= dlater + pas_angle; end if;
--QQ      if c(turn_lateral_graduated) then dlater:= dlater + pas_angle*Real(gx); end if;
      if Commanded (Turn_up)     then dverti:= dverti - pas_angle; end if;
      if Commanded (Turn_down)   then dverti:= dverti + pas_angle; end if;
      if Commanded (Swing_Left)  then dswing:= dswing + pas_angle; end if;
      if Commanded (Swing_Right) then dswing:= dswing - pas_angle; end if;
      if Do_Once (Quit) then escape:= true; end if;

      vect_dir:= Transpose(World_rotation) * (0.0,0.0,1.0);
      Eye:= Eye + vitesse * vect_dir;
      World_rotation:= XYZ_rotation( dverti,dlater,dswing ) * World_rotation;

      Orthonormalize(World_rotation);
      vitesse:= vitesse * attenu; -- stabilisation
      dswing:=  dswing  * attenu;
      dlater:=  dlater  * attenu;
      dverti:=  dverti  * attenu;

      radial_light_source(1):= Eye; -- lanterne
      Rotate_lights;
      
      if abs(dswing) + abs(dlater) + abs(dverti) > Real'epsilon then
        for r in laby'range loop
          laby(r).sorting_refreshed:= False;
        end loop;
      end if;

      for r in laby'range loop
        laby(r).projection_refreshed:= False;
      end loop;

    end Common_ops;

  begin
--    if MouseInstalled then
--      G_Initialize( 0, 0, X_Max, Y_Max );
--      MouseXY(centrX,centrY);
--      HideMouse;
--    end if;

    transparency_color:= Game_transparency_color;

    Init_Engine( X_res=> Graphics.Modes.Get_Width,
                 Y_res=> Graphics.Modes.Get_Height,
                 X_clip_left=>   0, X_clip_right=>  Graphics.Modes.Get_Width - 1 -   0,
                 Y_clip_top=>    0, Y_clip_bottom=> Graphics.Modes.Get_Height - 1 -  32,
                 Z_clip_min=>  fl_screen_virtual_size * 0.05,
                 X_offset=> Graphics.Modes.Get_Width/2,
                 Y_offset=> Graphics.Modes.Get_Height/2,
                 Focal=> 1.5*fl_screen_virtual_size );

    Eye:= (0.0,0.0,fl_screen_virtual_size);
    World_rotation:= XYZ_rotation( 0.5*pi,0.0,0.0 );
    Calculate_intensity_map( Game_intensities );
    Reset_lightings;
    Graphics.Colors.Set_Palette( Game_palette );
--    Multi_keys.Install;
--    Fine_Timer.Install;
--    Fine_Timer.Set_timer_frequency( freq );

    loop
      Common_ops;
--      Draw(Buffer.all, laby(1).all, auto, auto, Gouraud);
      Draw(laby(1).all, auto, auto, Gouraud);
      exit when escape;
    end loop;

--    Fine_Timer.Uninstall;
    Keyboard.Return_To_Normal_State;

  exception
    when others =>
--      Fine_Timer.Uninstall;
      Keyboard.Return_To_Normal_State;
      Graphics.Modes.Text_Mode;
      raise;
  end Demo_3D_01_in_graphics;

 stop: boolean:= false;
 can_draw: boolean;
 nres: integer;
 mode: Graphics.Modes.Mode_Type;

 begin
   Graphics.Initialize;

   begin
      Mouse.Initialize;
      Mouse_Installed := True;
   exception
      when Mouse.Mouse_Error =>
         Mouse_Installed := False;
   end;

  Put("Unpacking data...");

  Unzip.Load_zip_info( zip, zif );
  -- Load textures
  for tid in texture_id loop
    Load_zipped_BMP_texture( texture_id'image(tid) & ".bmp", tex(tid) );
  end loop;
  Unzip.Delete_zip_info( zif );

  Put_Line(" done.");

  Put("Init things...");
  Vehic001.Init;
  Shuttle3.Init;
  Init_laby;
  Put_Line(" done.");
  New_Line;
 
  Put_Line("[Mouse installed] " & boolean'image(Mouse_Installed));
  New_Line;
  Put_Line("[Keyboard]  Esc       : exit");
  Put_Line("            Left/Right: turn left/right");
  Put_Line("            Up/Down   : forward/backward");
--  Put_Line("            " & Key_image(31,true) & '/' & Key_image(32,true) &
--           "       : swing angle");  -- US: S/D
  Put_Line("            S/D       : swing angle");  -- US: S/D
--  Put_Line("            " & Key_image(30,true) & '/' & Key_image(44,true) &
--           "       : turn up/down"); -- US: A/Z
  Put_Line("            A/Z       : turn up/down"); -- US: A/Z
--  Put_Line("            " & Key_image(45,true) &
--           "         : snapshot to snap_???.bmp"); -- US: X
  Put_Line("            X         : snapshot to snap_???.bmp"); -- US: X
  New_Line;

   loop
      declare
         Modes : Graphics.Modes.Mode_Array := Graphics.Modes.Available_Modes;
      begin
         for I in Modes'Range loop
            Ada.Integer_Text_IO.Put (I, Width => 0);
            Put (") ");
            Put_Line(Graphics.Modes.To_String (Modes (I)));
         end loop;
         Put_Line("0) Quit");
         -- Switch to graphics mode
         can_draw:= true;
         begin
            Put("Choice: "); Get(nres); Skip_Line; New_Line;

            if nres in Modes'Range then
               mode := Modes (nres);
               Graphics.Modes.Set_Mode (mode);
            elsif nres = 0 then
               stop := true;
            else
               Put_Line("Bad choice..."); can_draw:= false;
            end if;
         exception
            when Data_error =>
               Skip_Line; Put_Line("Valid number, please..."); can_draw:= false;
            when others =>
               Put_Line("Unsupported display type or something else...");
               can_draw:= false;
         end;
         exit when stop;

         if can_draw then
            Demo_3D_01_in_graphics;
            Graphics.Modes.Text_Mode;
         end if;
      end;
   end loop;

   Mouse.Finalize;
   Graphics.Finalize;
exception
   when others =>
      Mouse.Finalize;
      Graphics.Finalize;
      raise;
end Demo_3D_01;
