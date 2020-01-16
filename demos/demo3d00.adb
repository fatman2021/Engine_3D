------------------------------------------------------------------------------
--  File:            Demo3D00.adb
--  Description:     Demo/test for Engine_3D package
--  Date / Version:  29.VIII.1999
--  Author:          Gautier.deMontmollin@Maths.UniNe.CH
--
--  Based on Peroxide / Telemachos tutorial #4 & Pascal sample
------------------------------------------------------------------------------

with Screen;
with Screen.IO;                         use Screen.IO;
with Screen.Effects;                    use Screen.Effects;
with Screen.Effects.IO;                 use Screen.Effects.IO;
with Game_Colors;                       use Game_Colors;

with Graphics.Colors;
with Graphics.Drawing;
with Graphics.Modes;

with Engine_3D;                         use Engine_3D;
with Engine_3D.Math;                    use Engine_3D.Math;
with Engine_3D.Merging;                 use Engine_3D.Merging;
with Engine_3D.Morphing;                use Engine_3D.Morphing;

with Icosaedron, Vehic001, X29, Shuttle3;

--with Multi_keys;                        use Multi_keys;
with Timer;
with Keyboard;
with Mouse;

with Unzip;                             use Unzip;
with Unzip.Streams;                     use Unzip.Streams;

with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Calendar;                      use Ada.Calendar;
with Ada.Exceptions;
with Ada.Numerics;                      use Ada.Numerics;
with Ada.Numerics.Elementary_functions; use Ada.Numerics.Elementary_functions;

procedure Demo_3D_00 is

  package IIO is new Integer_IO(integer);  use IIO;
  package FIO is new Float_IO(float);      use FIO;

  subtype Real is Engine_3D.Real; -- (and not Screen.Effects.Real)
  package RIO is new Float_IO(Real);       use RIO;

  ----------------------------
  -- Some constant switches --
  ----------------------------

  Mouse_Installed : Boolean;

  synchronized_rotations: constant boolean:= true;
  -- true: uses fine timing package

   type Command_Type is (
     Go_Backward,
     Go_Forward,
     Next_Demo,
     Rotate_Faster,
     Rotate_Slower,
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
     Next_Demo     => Keyboard.Space,
     Rotate_Faster => Keyboard.KP_Add,
     Rotate_Slower => Keyboard.KP_Subtract,
     Swing_Left    => Keyboard.S,
     Swing_Right   => Keyboard.D,
     Take_Snapshot => Keyboard.X,
     Turn_Down     => Keyboard.Z,
     Turn_Left     => Keyboard.Left,
     Turn_Right    => Keyboard.Right,
     Turn_Up       => Keyboard.A,
     Quit          => Keyboard.Escape
   );

   function Do_Once (Command : Command_Type) return Boolean is
   begin
      return Keyboard.Was_Struck (Key (Command));
   end Do_Once;

  -- for debugging via log-file
--  make_log: constant boolean:= false;
  make_log: constant boolean:= True;
  log_name: constant string:= "3d.log";
  log_file: file_type;

  -- FPS result file
  fps_file_name: constant string:= "demo3d00.fps";
  fps_file: file_type;

  side_left_tex, side_right_tex,
  wing_left_tex, wing_right_tex: p_Texture_map; -- textures for shuttle
 
  type Texture_list is array(natural range <>) of p_Texture_map;

  empty_tex: Texture_list(1..0);
  Doom_tex, Duke3D_tex, checker_tex: Texture_list(1..6);
  Sky_tex, Psc_tex, Torus_tex, Grid_tex: Texture_list(1..1);

  Doom_pal, Sky_pal, Psc_pal, multi_pal, spare_pal: Graphics.Colors.Palette_Type (Graphics.Colors.Color_Index);

  zip: constant string:= "demo3d00.dat";
  zif: Unzip.zip_info; -- zip directory structure for fast access

  procedure Load_zipped_BMP_texture(name: String; Texture: out p_Texture_map) is
    f: Zipped_File_Type;
    begin
      Open(f,zif,name);
      Load_BMP_texture( Stream(f), Texture );
      Close(f);
    end Load_zipped_BMP_texture;

--  procedure Load_zipped_BMP_palette(name: String; Palette:  out Color_Palette) is
  procedure Load_zipped_BMP_palette(
    name: String;
    Palette:  out Graphics.Colors.Palette_Type
  ) is
    f: Zipped_File_Type;
--    width: X_Loc; height: Y_Loc;
    width: Integer; height: Integer;
    begin
      Open(f,zif,name);
      Read_BMP_Header( Stream(f), width, height);
      Load_BMP_palette( Stream(f), Palette );
      Close(f);
    end Load_zipped_BMP_palette;

  procedure Prepare_checker_textures is
--    c: Color_type;
    c: Graphics.Colors.Color_Index;
    begin
      for i in checker_tex'range loop
        declare
          extra: constant integer:= 6-i;
          shift: constant integer:= 2**extra;
          bits: constant integer:= i+1+extra; -- 1 -> 2x2 checker table
          lmax: constant integer:= 2**bits - 1;
        begin
          checker_tex(i):= New Texture_map( bits, bits );
          for ix in 0..lmax loop
           for iy in 0..lmax loop
            if ((ix/shift) mod 2) = ((iy/shift) mod 2) then
--             c:= Standard_white;
             c:= 15;
            else
--             c:= Color_type (16 + 20 + (i-1)*40);
             c:= Graphics.Colors.Color_Index (16 + 20 + (i-1)*40);
            end if;
            Set_Pixel( checker_tex(i).all, ix, iy, c);
           end loop;
          end loop;
        end;
      end loop;
    end Prepare_checker_textures;

 pala: constant:= 16; palz: constant:= 255; palaz: constant:=palz-pala;

  ----------
  -- Cube --
  ----------

  cube: Object_3D( Max_points=> 8, Max_faces=> 6 );
  z_cube_max: integer;

  Procedure Init_cube is
   use type Graphics.Colors.Color_Index;
   t: integer:= screen_virtual_size / 5;
--   c: constant Color_Type:= standard_black; -- dummy colour
   c: constant Graphics.Colors.Color_Index := 0; -- dummy colour
   intens_min: constant intensity:= intensity'first;
   intens_max: constant intensity:= intensity'last;
   degr: degrade_description(1..6);
   begin
     z_cube_max:= 1 + integer( float(t) * 1.732 );

     cube.baseobj:=
      ( (-t,-t,-t), ( t,-t,-t), (-t, t,-t), ( t, t,-t),
        (-t,-t, t), ( t,-t, t), (-t, t, t), ( t, t, t));

     cube.faces:=
      ( ((1,3,4,2), true,  null, 1,1, intens_min,intens_max, c,c),
        ((2,4,8,6), false, null, 1,1, intens_min,intens_max, c,c),
        ((5,6,8,7), true,  null, 1,1, intens_min,intens_max, c,c),
        ((1,5,7,3), false, null, 1,1, intens_min,intens_max, c,c),
        ((1,2,6,5), true,  null, 1,1, intens_min,intens_max, c,c),
        ((3,7,8,4), false, null, 1,1, intens_min,intens_max, c,c));

     cube.center:= (0.0,0.0,fl_screen_virtual_size);

     -- We will update it when displaying textures and colours together
     for f in 1..6 loop
--       cube.faces(f).color_min:=  Color_type(pala + (f-1) * 40);
       cube.faces(f).color_min := Graphics.Colors.Color_Index (pala + (f-1) * 40);
       cube.faces(f).color_max:=  cube.faces(f).color_min + 39;
       degr(f):= ( cube.faces(f).color_min, 39 );
     end loop;

     Init_object(cube);
     Calculate_intensity_map( degr );  -- used by checker texture

   end Init_cube;

  -----------
  -- Torus --
  -----------

  nga: constant:= 13;
  npa: constant:= 9;

  torus: Object_3D( Max_points=> nga*npa, Max_faces=> nga*npa );

  Procedure Init_torus is
   gr: constant float:= fl_screen_virtual_size * 0.3;  -- big radius
   pr: constant float:= gr * 0.2;  -- small radius
   ga, pa, rr: float;
   ind, p1,p2,p3,p4: integer;
   intens_min: constant intensity:= (22 * intensity'first) / 100;
   intens_max: constant intensity:= (22 * intensity'last)  / 100;
   begin
     for cp in 0..npa-1 loop
       pa:= 2.0*pi * float(cp) / float(npa);
       for cg in 0..nga-1 loop
         ga:= 2.0*pi * float(cg) / float(nga);
         ind:= 1 + cg + nga * cp;
         rr:= gr + pr * cos(pa);

         torus.baseobj( ind ):=
           (      integer(rr * cos(ga)),
                  integer(rr * sin(ga)),
                  integer(pr * sin(pa)) );

         p1:= cg             + nga * cp;
         p2:= (cg+1) mod nga + nga * cp;
         p3:= (cg+1) mod nga + nga * ((cp+1) mod npa);
         p4:= cg             + nga * ((cp+1) mod npa);

         torus.faces( ind ):=
           ((1+p1,1+p2,1+p3,1+p4), true,  null, 1,1, intens_min,intens_max, 2,2);
       end loop;
     end loop;

     torus.center:= (0.0,0.0,fl_screen_virtual_size);

     Init_object(torus);

   end Init_torus;

  -------------------------
  -- Multiple icosadrons --
  -------------------------

  multico: p_Object_3D;

  Procedure Init_multico is
    use Icosaedron;
    d: constant:= fl_screen_virtual_size * 0.4; -- a certain distance
    ico_centered: Object_3D:= Icosaedron_obj.all;
    begin
      ico_centered.center:= (0.0,0.0,0.0);
      declare
        ico_textured: Object_3D:= ico_centered;
      begin
        for i in 1..ico_textured.Num_of_faces loop
          ico_textured.faces(i).textured:= true;
          ico_textured.faces(i).rep_U:= 10;
          ico_textured.faces(i).rep_V:= 10;
        end loop;

       multico:= 0.5 * ico_centered +      -- original icosaedron
         1.0 * ico_textured +              -- plus a textured one
         (0.1 * ico_centered + Vector3'(d,0.0,0.0)) + -- plus "moons"
         (0.2 * ico_centered + Vector3'(-0.5*d, 0.866*d,0.0)) +
         (0.3 * ico_centered + Vector3'(-0.5*d,-0.866*d,0.0));

        multico.center:= (0.0,0.0,fl_screen_virtual_size);
      end;
    end Init_multico;

  ---------------------------
  -- Waves, with triangles --
  ---------------------------
  package Waves is
    waves_obj: Engine_3D.p_Object_3D;
    procedure Init;
  end Waves;

  package body Waves is
    Procedure Init is
      nf: constant:= 6;
      nsub: constant array(1..6) of positive:= (1,3,1,3,2,2);

      np,ns: natural;

      ta: constant float:= float(screen_virtual_size) * 0.2;
      ind0, p1,p2,p3,p4: integer;
      x,y,z,vx,vy,vz: float;
      intens_min: constant intensity:= (22 * intensity'first) / 100;
      intens_max: constant intensity:= (22 * intensity'last)  / 100;
      begin
        ns:= 0;
        np:= 0;
        for cf in 1..nf loop
          np:= np + 2*nsub(cf)**2;
          ns:= ns + (nsub(cf)+1)**2;
        end loop;

        waves_obj:=
          New Object_3D( Max_points=> ns, Max_faces=> np );

        ns:= 0;
        np:= 0;
        for cf in 1..nf loop -- faces

          for cy in 0..nsub(cf) loop
            y:= (float(cy) / float(nsub(cf))) * 2.0 - 1.0;
            for cx in 0..nsub(cf) loop
              x:= (float(cx) / float(nsub(cf))) * 2.0 - 1.0;
              z:= -0.5 * ( x + 1.0 ) * ( x - 1.0 ) * ( cos( 0.5*pi*y ) );

              case cf is
                when 1 => vx:=  x;        vy:= -1.0 - z; vz:=  y;
                when 2 => vx:=  1.0 + z;  vy:=  x;       vz:=  y;
                when 3 => vx:= -x;        vy:=  1.0 + z; vz:=  y;
                when 4 => vx:= -1.0 - z;  vy:= -x;       vz:=  y;
                when 5 => vx:= -x;        vy:=  y;       vz:= -1.0 - z;
                when 6 => vx:=  x;        vy:=  y;       vz:=  1.0 + z;
                when others => null;
              end case;

              ind0:= cx + (nsub(cf)+1) * cy + ns;

              waves_obj.baseobj( ind0 + 1 ):=
                   ( integer(ta * vx),
                     integer(ta * vy),
                     integer(ta * vz) );

              if cx<nsub(cf) and then cy<nsub(cf) then
                p1:= ind0 + 1;
                p2:= p1 + 1;
                p3:= p2 + (nsub(cf)+1);
                p4:= p1 + (nsub(cf)+1);

                ind0:= 2*(cx + nsub(cf) * cy) + np;
                waves_obj.faces( 1 + ind0):=
                  ((p1,p2,p3, 0), true,  null, 1,1,
                                          intens_min,intens_max, 2,2);
                waves_obj.faces( 2 + ind0):=
                  ((p1, 0,p3,p4), true,  null, 1,1,
                                          intens_min,intens_max, 2,2);

              end if;

            end loop;
          end loop;
          np:= np + 2*nsub(cf)**2;
          ns:= ns + (nsub(cf)+1)**2;
        end loop;

        waves_obj.center:= fl_screen_virtual_size * (0.0,0.0,1.0);

        Init_object(waves_obj.all);

      end Init;
    end Waves;

  procedure Prepare_shuttle is
    use Shuttle3;
  begin
    Init;
    declare
      f: Face_array renames shuttle3_obj.Faces;
    begin
      f(wing_right_a).texture:=  wing_right_tex;
      f(wing_right_a).textured:= true;
      f(wing_right_b).texture:=  wing_right_tex;
      f(wing_right_b).textured:= true;
      f(wing_left_a).texture:=  wing_left_tex;
      f(wing_left_a).textured:= true;
      f(wing_left_b).texture:=  wing_left_tex;
      f(wing_left_b).textured:= true;
      f(side_left).texture:=  side_left_tex;
      f(side_left).textured:= true;
      f(side_right).texture:=  side_right_tex;
      f(side_right).textured:= true;
    end;
  end Prepare_shuttle;

 procedure Demo_3D_00_in_graphics is

--  background_colour: color_type;
  background_colour: Graphics.Colors.color_index;

  pseudo_parallel_src: array(1..max_parallel_lights) of Point;

  procedure Reset_lightings is
    begin
      parallel_lights:= 0;
    end;

  procedure Modify_latest_light(x,y: integer) is
    begin
      if parallel_lights >0 then
        pseudo_parallel_src(parallel_lights):= (x,y,screen_virtual_size);
        Find_light_direction( pseudo_parallel_src(parallel_lights),
                              (0,0,2*screen_virtual_size), 
                              parallel_light_vect(parallel_lights) );
        parallel_light_force(parallel_lights):= 1.0;
      end if;
      if make_log then
       Open(log_file, append_file, log_name);
       Put_Line(log_file, "//lights:");
       for i in 1..parallel_lights loop
         Put(log_file, i,3);
         Put(log_file,"  x:"); Put(log_file,parallel_light_vect(i)(1),3,5,0);
         Put(log_file,"  y:"); Put(log_file,parallel_light_vect(i)(2),3,5,0);
         Put(log_file,"  z:"); Put(log_file,parallel_light_vect(i)(3),3,5,0);
         New_Line(log_file);
       end loop;
       New_Line(log_file);
       Close(log_file);
      end if;
    end;

  procedure Add_light(x,y: integer) is
    begin
      if parallel_lights < max_parallel_lights then
        parallel_lights:= parallel_lights + 1;
        Modify_latest_light(x,y);
        parallel_light_force(parallel_lights):= 1.0;
      end if;
    end;

 Procedure Multicolor is
      use Graphics.Colors;
--   c, s, ccol: Color_Type;
   c, ccol: Color_Index;
   s : Intensity_Type;
--   subpal: constant Color_Type:= (palaz+1) / 6;
   subpal: constant Color_Index:= (palaz+1) / 6;
   begin
--     for i in Color_type'(pala) .. Color_type'(palz) loop
     for i in Color_Index'(pala) .. palz loop
       c:= i - pala;
--       s:=      Color_type(63.0 * float(c mod subpal) / float(subpal-1));
       s:= Intensity_Type (float(c mod subpal) / float(subpal-1)); --QQ Was 63 *
       ccol:=   c  /  subpal;
       case ccol is
         when 0=> Multi_pal(i):= To_Color((s,s,s)); -- grey scale
         when 1=> Multi_pal(i):= To_Color((s,0.0,0.0)); -- red scale
         when 2=> Multi_pal(i):= To_Color((0.0,s,0.0)); -- green scale
         when 3=> Multi_pal(i):= To_Color((0.0,0.0,s)); -- blue scale
         when 4=> Multi_pal(i):= To_Color((0.0,s,s)); -- cyan scale
         when 5=> Multi_pal(i):= To_Color((s,0.0,s)); -- magenta scale
         when others => null;
       end case;
     end loop;
   end;

-- Procedure PurplePal is
--      use Graphics.Colors;
--   begin
----     for i in Color_type'(0) .. 63 loop
----       Set_Color(i + pala, (i,0,i));   -- 63 shades from black to purple
--     for i in Intensity_Type loop
--       Set_Color(i + pala, To_Color ((i,0,i)));   -- 63 shades from black to purple
--     end loop;
--   end;

-- Procedure FakePhongPal is
--      use Graphics.Colors;
--   begin
----     for i in Color_type'(0) .. 63 loop
----       Set_Color(i + pala, (i, 10+Color_type(float(i)/1.4), 
----                               20+Color_type(float(i)/1.6) ) );
--     for i in Intensity_Type loop
--       Set_Color(i + pala, To_Color (i, 10+Intensity_Type(float(i)/1.4), 
--                               20+Intensity_Type(float(i)/1.6) ) );
--     end loop;
--   end;

  -- Fine timing
  freq: constant:= 600.0; -- Hz

  -- Frames-per-second recording
  
  T1,T2: time;     -- "Ada" clock for non-synchronized mode
  E1,E2: float;    -- Fine_time times in synchronized mode
  frames: natural;
  rec: natural:= 0;

  type FPS_info is record
    rate: float;
    name: string(1..100);
    nlen: natural;
  end record;
    
  FPS: array(1..100) of FPS_info;
  
  procedure Reset_FPS is
    begin
      frames:= 0;
      if synchronized_rotations then
        E1:= float(Timer.Clock);
      else
        T1:= Clock;
      end if;
    end;
  
  procedure Store_FPS(name: String) is
    secs: float;
    begin
      if synchronized_rotations then
        E2:= float(Timer.Clock);
        secs:= E2-E1;
      else
        T2:= Clock;
        secs:= float(T2-T1);
      end if;
      rec:= rec + 1;
      if secs= 0.0 then
        FPS(rec).rate:= 0.0;
      else
        FPS(rec).rate:= float(frames) / secs;
      end if;
      FPS(rec).name(1..name'length):= name;
      FPS(rec).nlen:= name'length;
    end;

  -- This is an off-screen frame buffer
--  type Frame_Access is access Screen_Buffer;
--  Buffer : Frame_Access;    -- The buffer

  procedure Show_Lights is
      use Graphics.Drawing;
    PA,PB: ScrPoint; ok: boolean;
    begin
      for i in 1 .. parallel_lights loop
        Project( pseudo_parallel_src(i), PA, ok);
        Project( (0,0,2*screen_virtual_size), PB, ok);
--        Line( buffer.all, X_loc(PA.x),Y_loc(PA.y), X_loc(PB.x),Y_loc(PB.y) );
        Line(PA.x, PA.y, PB.x, PB.y, 15);
      end loop;
    end;

  Xrot,Yrot, Zrot,                  -- object rotation
  Xrotl,Yrotl, Zrotl : integer:= 0; -- light rotation

  previously_left_button_pressed: boolean:= false;

  procedure Mouse_and_lights is
     use Mouse;
    btns: Press_Array;
    mx,my: Integer;
    Spare : Boolean;
    begin
      if Mouse_Installed then     -- Mouse, for lightings
        Spare := Update_State;
        Get_State ( mx, my, btns );

        if btns( left ) then
--          mx:= mx - X_Size/2;
--          my:= my - Y_size/2;
--          mx:= (screen_virtual_size * mx) / Y_Size;
--          my:= (screen_virtual_size * my) / Y_Size;
          mx:= mx - Graphics.Modes.Get_Width/2;
          my:= my - Graphics.Modes.Get_Height/2;
          mx:= (screen_virtual_size * mx) / Graphics.Modes.Get_Width;
          my:= (screen_virtual_size * my) / Graphics.Modes.Get_Height;
          if previously_left_button_pressed then
            Modify_latest_light(mx,my);
          else
            Add_light(mx,my);
          end if;
          Show_lights;
        elsif btns( right ) then
          Reset_lightings;
        end if;

        previously_left_button_pressed:= btns( left );  
      end if;
    end Mouse_and_lights;


  escape, skip_to_next: boolean;
  snaps: natural:= 0;

  speed: float:= 40.0;
  speed_factor: constant float:= 2.0 ** (1.0/1.666); -- 1.666 sec. -> 2x speed
  totrot: float:= 0.0;
  last_time: float:= 0.0;

  procedure Common_ops(o: in out Object_3D) is
    elaps, fine_time: float;
    ext: String(1..4);
    rota: constant:= 0.5*pi;
    pas: Real;

    function Commanded (Command : Command_Type) return Boolean is
      begin
        if synchronized_rotations then
          return Keyboard.Is_Pressed (Key (Command)); -- we can proportion
        else
          return Do_Once (Command); -- we can't -> separate strikes
        end if;
      end;
    
    begin
      Mouse_and_lights;
      Graphics.Copy_Graphics_To_Screen;

      Keyboard.Update;

--      if Strike_1( 45 ) then -- 'x' on US and other keyboards
      if Do_Once (Take_Snapshot) then
        snaps:= snaps + 1;
        Put(To=> ext, Item=> 1000 + snaps); 
--        Save_BMP("snap_" & ext(2..4) & ".bmp", Buffer.all, Get_Palette);
        Graphics.Colors.Get_Palette (Spare_pal);
        Save_BMP("snap_" & ext(2..4) & ".bmp", Spare_pal);
      end if;

      Graphics.Drawing.Clear_Graphics (background_colour);  --  Clear the buffer

      o.auto_rotation:=
         XYZ_rotation( Engine_3D.Real(Xrot)*pi/1800.0,
                       Engine_3D.Real(Yrot)*pi/1800.0,
                       Engine_3D.Real(Zrot)*pi/1800.0 );

      if synchronized_rotations then
        -- Real-time synchronized speed
        fine_time:= float(Timer.Clock);
        elaps:= fine_time - last_time;

        totrot:= totrot + elaps * speed;
        while totrot > 3600.0 loop totrot:= totrot - 3600.0; end loop;
        Xrot  := integer(totrot *  9.0) mod 3600;
        Yrot  := integer(totrot * 21.0) mod 3600;
        Zrot  := integer(totrot *  7.0) mod 3600;

        last_time:= fine_time;

      else  -- unsynchronized mode

        Xrot  := (Xrot + 9)  mod 3600;
        Yrot  := (Yrot + 21) mod 3600;
        Zrot  := (Zrot + 7)  mod 3600;

        elaps:= 1.0;
      end if;

      escape := Do_Once (Quit);
      skip_to_next := Do_Once (Next_Demo);
--      if Strike_1( key_space ) then
--        skip_to_next:= true;
--      end if;

--      if take_key( key_gray_plus ) then  -- * speed_factor after 1 second
      if Commanded (Rotate_Faster) then  -- * speed_factor after 1 second
         speed:= Exp( Log(speed) + Log(speed_factor) * elaps );
      end if;
--      if take_key( key_gray_minus ) then -- / speed_factor after 1 second
      if Commanded (Rotate_Slower) then -- / speed_factor after 1 second
         speed:= Exp( Log(speed) - Log(speed_factor) * elaps );
      end if;
--      if take_key( key_left ) then 
      if Commanded (Turn_Left) then
         World_rotation:= XYZ_rotation( 0.0, -rota*Real(elaps),0.0 ) *
           World_rotation;
      end if;
--      if take_key( key_right ) then 
      if Commanded (Turn_Right) then
         World_rotation:= XYZ_rotation( 0.0, +rota*Real(elaps),0.0 ) *
           World_rotation;
      end if;
--      if take_key( 31 ) then -- swing -
      if Commanded (Swing_Left) then
         World_rotation:= XYZ_rotation( 0.0,0.0,-rota*Real(elaps) ) *
           World_rotation;
      end if;
--      if take_key( 32 ) then -- swing +
      if Commanded (Swing_Right) then
         World_rotation:= XYZ_rotation( 0.0,0.0,+rota*Real(elaps) ) *
           World_rotation;
      end if;
--      if take_key( 30 ) then -- turn up
      if Commanded (Turn_Up) then
         World_rotation:= XYZ_rotation( -rota*Real(elaps),0.0,0.0 ) *
           World_rotation;
      end if;
--      if take_key( 44 ) then -- turn down
      if Commanded (Turn_Down) then
         World_rotation:= XYZ_rotation( +rota*Real(elaps),0.0,0.0 ) *
           World_rotation;
      end if;
      pas:= fl_screen_virtual_size * Real(elaps);
--      if take_key( key_up ) then
      if Commanded (Go_Forward) then
         Eye:= Eye + pas * (Transpose(World_rotation) * (0.0,0.0,1.0));
      end if;
--      if take_key( key_down ) then
      if Commanded (Go_Backward) then
         Eye:= Eye - pas * (Transpose(World_rotation) * (0.0,0.0,1.0));
      end if;

      Orthonormalize(World_rotation);
      Rotate_lights;
      o.sorting_refreshed:= False;

    end Common_ops;

   procedure Texture_demo(
     title: string; 
     o: in out Object_3D; tl: Texture_list;
--     transp: Color_type; P: Color_Palette;
     transp: Graphics.Colors.Color_Index; P: Graphics.Colors.Palette_Type;
     sh: shading_mode;
     surf_select: surface_select;
     mm: texture_mapping_mode ) is

     texture_index: positive;

     begin
       if tl'length /=0 then -- non-empty texture list:
         -- We set one texture to one face:
         texture_index:= tl'first;
         for f in 1..o.Num_of_faces loop
           o.Faces(f).texture:= tl(texture_index);
           texture_index:= texture_index + 1;
           if texture_index > tl'last then texture_index:= tl'first; end if;
         end loop;
       end if;

--       transparency_color:= transp;
       transparency_color:= transp;
--       Set_Palette(P);
       Graphics.Colors.Set_Palette(P);
       Reset_FPS;
       loop
        Common_ops(o);
        case mm is
          when npersp_y_affine_x =>    Yrot:= 0;   Zrot:= 0;
          when affine_y_npersp_x =>    Xrot:= 0;   Zrot:= 0;
          when others => null;
        end case;

--        Draw(Buffer.all, o, surf_select, mm, sh);
        Draw(o, surf_select, mm, sh);
        exit when skip_to_next or escape;
        frames:= frames + 1;
       end loop;
       Store_FPS(title &
                 integer'image(o.num_of_points) & " points" &
                 integer'image(o.num_of_faces)  & " polys" &
                 ' ' & surface_select'image( surf_select ) &
                 '/' & texture_mapping_mode'image(mm) &
                 '/' & shading_mode'image(sh));
     end Texture_demo;

  begin
--    if MouseInstalled then
--      G_Initialize( 0, 0, X_Max, Y_Max );
--      G_Initialize (
--        0,
--        0,
--        Graphics.Modes.Get_Width - 1,
--        Graphics.Modes.Get_Height - 1
--      );
--      HideMouse;
--    end if;

--    background_colour:= standard_black;
    background_colour:= 0;

--    Init_Engine( X_res=> X_Size,
--                 Y_res=> Y_Size,
--                 X_clip_left=> 0, X_clip_right=>  X_Max,
--                 Y_clip_top=>  0, Y_clip_bottom=> Y_Max,
--                 Z_clip_min=>  fl_screen_virtual_size * 0.1,
--                 X_offset=> X_Size/2,
--                 Y_offset=> Y_Size/2,
--                 Focal=> fl_screen_virtual_size );

    Init_Engine( X_res=> Graphics.Modes.Get_Width,
                 Y_res=> Graphics.Modes.Get_Height,
                 X_clip_left=> 0, X_clip_right=>  Graphics.Modes.Get_Width - 1,
                 Y_clip_top=>  0, Y_clip_bottom=> Graphics.Modes.Get_Height - 1,
                 Z_clip_min=>  fl_screen_virtual_size * 0.1,
                 X_offset=> Graphics.Modes.Get_Width/2,
                 Y_offset=> Graphics.Modes.Get_Height/2,
                 Focal=> fl_screen_virtual_size );

    Eye:= (others=> 0.0);   -- eye (again) at centre of the universe...
    World_rotation:= Id33;

    Reset_lightings;
    Add_light(500,250);
--    Multi_pal:= Get_Palette; -- to get the standard values 0..15
    Graphics.Colors.Get_Palette (Multi_pal); --QQ do this portably

    -------------- Test all shading methods:
    --  type shading_mode is ( none, Z_shading, Lambert, Gouraud, Phong );

    Multicolor;
    Graphics.Colors.Set_Palette( Multi_pal );

    -- reset cube data (orignal palette + texture repetition)
    Init_cube;

--    Multi_keys.Install;

--    if synchronized_rotations then
--      Fine_Timer.Install;
--      Fine_Timer.Set_timer_frequency( freq );
--    end if;

    for shading_method in shading_mode loop
      Reset_FPS;
      loop
       Common_ops(cube);

       Draw (cube, colors_only, affine_y_affine_x,
             shading_method, true, 
             screen_virtual_size - z_cube_max * 2,
             screen_virtual_size + z_cube_max * 2);

       exit when skip_to_next;
       frames:= frames + 1;
      end loop;
      Store_FPS("Shading method: " & shading_mode'image(shading_method) );
    end loop;

    --------------------------
    -- Test texture mapping --
    --------------------------

    Reset_intensity_map;

    -- Comparision affine / persp texture with a test picture
    for mm in texture_mapping_mode loop
      Texture_demo( "Checker", cube, checker_tex,    2,   multi_pal, Z_only,
                        textures_only, mm );
    end loop;

    -- Display all these nice pictures on our cube...
    Texture_demo( "Sky", cube, sky_tex,    0,   sky_pal, Z_only,
                      textures_only, auto );

    ---------------------
    -- Shaded textures --
    ---------------------
    
    Calculate_intensity_map( Doom_intensities );

    -- Suitable intensity percentage ranges:
    for i in 1..cube.num_of_faces loop
      cube.faces(i).intens_min:= (55 * intensity'first) / 100;
      cube.faces(i).intens_max:= (35 * intensity'last ) / 100;
    end loop;

    Texture_demo( "Doom", cube, Doom_tex, Doom_transparency_color,
                  Doom_pal, Lambert,
                  textures_only, auto);

    Texture_demo( "Doom", cube, Doom_tex, Doom_transparency_color,
                  Doom_pal, Gouraud,
                  textures_only, auto);

    Calculate_intensity_map( Duke3D_intensities );

    -- Suitable intensity percentage ranges:
    for i in 1..cube.num_of_faces loop
      cube.faces(i).intens_min:= (37 * intensity'first) / 100;
      cube.faces(i).intens_max:= (37 * intensity'last ) / 100;
    end loop;

    Texture_demo( "Duke 3D", cube, Duke3D_tex, Game_transparency_color,
                  Game_palette, Gouraud,
                  textures_only, auto);

    -- Test texture repetition
    for i in 1..cube.num_of_faces loop
      cube.faces(i).rep_U:= i;
      cube.faces(i).rep_V:= cube.num_of_faces+1-i;
    end loop;
    Texture_demo( "Duke 3D", cube, Duke3D_tex, Game_transparency_color,
                  Game_palette, Gouraud,
                  textures_only, auto);

    -- Mixed set textures and colours - on different faces!
    --  first, colour span redef. for the coloured faces:
    cube.faces(2).color_min:=  32;
    cube.faces(2).color_max:=  63;
    cube.faces(4).color_min:=  96;
    cube.faces(4).color_max:= 127;
    cube.faces(6).color_min:= 128;
    cube.faces(6).color_max:= 143;

    Texture_demo( "Duke 3D", cube, Duke3D_tex, Game_transparency_color,
                  Game_palette, Gouraud,
                  auto, auto );

    for FX in Gouraud .. Phong loop
      Texture_demo( "Icosaedron", Icosaedron.Icosaedron_obj.all, Torus_tex,
                    Game_transparency_color,
                    Game_palette, FX, auto, auto );
    end loop;

    Texture_demo( "Multi-icosaedron", multico.all, Grid_tex,
                  Game_transparency_color,
                  Game_palette, Gouraud, auto, auto );

    Texture_demo( "Torus", torus, Torus_tex, Game_transparency_color,
                  Game_palette, Gouraud, auto, auto );

    Texture_demo( "Water", Waves.waves_obj.all, Psc_tex, 0, Psc_pal,
                  Gouraud, auto, auto );

    background_colour:= 133; -- a dark red

    Texture_demo( "Zembla", Vehic001.vehicle_001.all, empty_tex,
                  Game_transparency_color,
                  Game_palette, Gouraud, auto, auto );

    Texture_demo( "X29", X29.X29_obj.all, empty_tex,
                  Game_transparency_color,
                  Game_palette, Gouraud, auto, auto );

    background_colour:= 69; -- a dark blue

    Texture_demo( "Shuttle3", Shuttle3.Shuttle3_obj.all, empty_tex,
                  Game_transparency_color,
                  Game_palette, Gouraud, auto, auto );

--    if synchronized_rotations then
--      Fine_Timer.Uninstall;
--    end if;
    Keyboard.Return_To_Normal_State;

    -- Output of FPS statistics
    for r in 1..rec loop
      Put(fps_file, FPS(r).rate, 4,2,0 );
      Put_Line(fps_file, "   - " & FPS(r).name(1..FPS(r).nlen));
    end loop;

  exception
    when others =>
--      if synchronized_rotations then Fine_Timer.Uninstall; end if;
      Keyboard.Return_To_Normal_State;
      Graphics.Modes.Text_Mode;
      raise;
  end Demo_3D_00_in_graphics;

 begin
   Graphics.Initialize;

   begin
      Mouse.Initialize;
      Mouse_Installed := True;
   exception
      when Mouse.Mouse_Error =>
         Mouse_Installed := False;
   end;

  if make_log then Create(log_file, name=> log_name); Close(log_file); end if;
  
  Put_Line("      ****************************************************************");
  Put_Line("      *                                                              *");
  Put_Line("      *                 3D OBJECT ENGINE - THE FILLS                 *");
  Put_Line("      *                        by : Telemachos                       *");
  Put_Line("      *                  Extended (Demo_3D_00) by GdM                *");
  Put_Line("      *                                                              *");
  Put_Line("      ****************************************************************");
  New_Line;
  Put_Line("      This demo is based on Peroxide Programming Tips #4");
  Put(     "      Unpacking now textures & palettes...");

  Unzip.Load_zip_info( zip, zif );
  -- Load the Doom Palette
  Load_zipped_BMP_palette ("caisse.bmp", Doom_pal);
  -- Load Doom textures
  Load_zipped_BMP_texture( "caisse.bmp",   Doom_tex(1) );
  Load_zipped_BMP_texture( "w111_2.bmp",   Doom_tex(2) );
  Load_zipped_BMP_texture( "wall71_5.bmp", Doom_tex(3) );
  Load_zipped_BMP_texture( "w109_2.bmp",   Doom_tex(4) );
  Load_zipped_BMP_texture( "door9_1.bmp",  Doom_tex(5) );
  Load_zipped_BMP_texture( "w94_1.bmp",    Doom_tex(6) );

  -- Load Duke3D textures
  Load_zipped_BMP_texture( "t004p000.bmp",  Duke3D_tex(1) );
  Load_zipped_BMP_texture( "t004p073.bmp",  Duke3D_tex(2) );
  Load_zipped_BMP_texture( "t004p077.bmp",  Duke3D_tex(3) );
  Load_zipped_BMP_texture( "t004p094.bmp",  Duke3D_tex(4) );
  Load_zipped_BMP_texture( "stones01.bmp",  Duke3D_tex(5) );
  Load_zipped_BMP_texture( "t004p120.bmp",  Duke3D_tex(6) );

  -- Load others
  Load_zipped_BMP_palette ("skyenv.bmp", sky_pal);
  Load_zipped_BMP_texture( "skyenv.bmp", sky_tex(1) );

  Load_zipped_BMP_texture( "t004p113.bmp", Torus_tex(1) );
  Grid_tex(1):= Duke3D_tex(1);

  Load_zipped_BMP_palette ("piscine2.bmp", psc_pal);
  Load_zipped_BMP_texture( "piscine2.bmp", psc_tex(1) );

  Load_zipped_BMP_texture( "sideleft.bmp", side_left_tex );
  Load_zipped_BMP_texture( "siderigh.bmp", side_right_tex );
  Load_zipped_BMP_texture( "wingleft.bmp", wing_left_tex );
  Load_zipped_BMP_texture( "wingrigh.bmp", wing_right_tex );

  Unzip.Delete_zip_info( zif );

  Put_Line(" done.");

  Put_Line("      Checker textures");
  Prepare_checker_textures;

  Put("      Initialize 3D Objects...");
  Init_cube;
  Init_torus;
  Waves.Init;
  Vehic001.Init;
  X29.Init;
  Prepare_shuttle;
  Icosaedron.Init;
  Init_multico;
  Put_Line(" done.");
 
  Put(     "      Shadings:  ");
  for s in shading_mode loop Put(' ' & shading_mode'image(s)); end loop;
  New_Line;
  New_Line;

  if Mouse_Installed then
    Put_Line("[Mouse]     Left button: Add / Move new light. Right button: reset lights");
  else
    Put_Line("Mouse not installed!");
  end if;
  New_Line;
  Put_Line("[Keyboard]  SPACE     : next part");
  Put_Line("            Gray +/-  : object rotation speed control");
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

  begin
    Open(fps_file, append_file, fps_file_name);
  exception
    when name_error => -- file doesn't exist
    Create(fps_file, out_file, fps_file_name);
  end;
  Put(fps_file, "Demo starting - ");
  Put(fps_file, Year(Clock),0);
  Put(fps_file, '.');
  Put(fps_file, Month(Clock),0);
  Put(fps_file, '.');
  Put(fps_file, Day(Clock),0);
  Put(fps_file, ", ");
  Put(fps_file, natural(Seconds(Clock)) / 3600,0);
  Put(fps_file, ':');
  Put(fps_file, (natural(Seconds(Clock)) mod 3600) / 60,0);
  New_Line(fps_file);

   declare
      Modes : Graphics.Modes.Mode_Array := Graphics.Modes.Available_Modes;
   begin
      Choose_Mode: loop
         for I in Modes'Range loop
            Ada.Integer_Text_IO.Put (I, Width => 0);
            Put (") ");
            Put_Line(Graphics.Modes.To_String (Modes (I)));
         end loop;
         Put_Line("0) Quit");

         -- Switch to graphics mode
         declare
            Choice : Integer;
         begin
            Put("Choice: ");
            Get(Choice);
            Skip_Line;
            New_Line;
   
            exit Choose_Mode when Choice = 0;

            if Choice in Modes'Range then
               Graphics.Modes.Set_Mode (Modes (Choice));
               Put_Line(
                 fps_file,
                 "New mode: " & Graphics.Modes.To_String (Modes (Choice))
               );
               Demo_3D_00_in_graphics;
               Graphics.Modes.Text_Mode;
            else
               Put_Line("Valid number, please...");
            end if;
         exception
            when Data_error =>
               Skip_Line; Put_Line("Valid number, please...");
            when E: others =>
               Put_Line("Unsupported display type or something else...");
               Put_Line (Ada.Exceptions.Exception_Information (E));
         end;
      end loop Choose_Mode;
   end;

  New_Line(fps_file);
  Close(fps_file);

  Put_Line("Frames-per-second are stored into file " & fps_file_name);

  -- Deallocation of big objects (unnecessary in this context)
  Dispose(Vehic001.vehicle_001);
  Dispose(X29.x29_obj);
  Dispose(Shuttle3.Shuttle3_obj);
  Dispose(Waves.waves_obj);

   Mouse.Finalize;
   Graphics.Finalize;
 exception
   when others =>
    if Is_Open(fps_file) then Close(fps_file); end if;
    Mouse.Finalize;
    Graphics.Finalize;
    raise;
 end Demo_3D_00;
