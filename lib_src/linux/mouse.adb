with Interfaces.C;

package body Mouse is -- linux
   use Interfaces.C;

   procedure vga_setmousesupport (stat : int);
   pragma Import (C, vga_setmousesupport);

   procedure mouse_close;
   pragma Import (C, mouse_close);

   function mouse_getbutton return unsigned;
   pragma Import (C, mouse_getbutton);

   function mouse_getx return int;
   pragma Import (C, mouse_getx);

   function mouse_gety return int;
   pragma Import (C, mouse_gety);

   function mouse_update return int;
   pragma Import (C, mouse_update);

   MOUSE_LEFTBUTTON   : constant := 4;
   MOUSE_MIDDLEBUTTON : constant := 2;
   MOUSE_RIGHTBUTTON  : constant := 1;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      mouse_close;
   end Finalize;

   ---------------
   -- Get_State --
   ---------------

   procedure Get_State (
     X, Y    : out Integer;
     Buttons : out Press_Array
   ) is
      Button_Flags : unsigned;
   begin
      X := Integer (mouse_getx);
      Y := Integer (mouse_gety);
      Button_Flags := mouse_getbutton;
      Buttons (Left)   := (Button_Flags and MOUSE_LEFTBUTTON) /= 0;
      Buttons (Right)  := (Button_Flags and MOUSE_RIGHTBUTTON) /= 0;
      Buttons (Middle) := (Button_Flags and MOUSE_MIDDLEBUTTON) /= 0;
   end Get_State;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      vga_setmousesupport (1);
   end Initialize;

   ------------------
   -- Update_State --
   ------------------

   function Update_State return Boolean is
   begin
      return mouse_update /= 0;
   end Update_State;
end Mouse;

