with PC_Mouse;

package body Mouse is -- DOS
   use PC_Mouse;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      null;
   end Finalize;

   ---------------
   -- Get_State --
   ---------------

   procedure Get_State (
     X, Y    : out Integer;
     Buttons : out Press_Array
   ) is
      btns : Mouse_button_bunch;
   begin
      PC_Mouse.Mouse (X, Y, btns);
      Buttons (Left) := btns (PC_Mouse.Left);
      Buttons (Right) := btns (PC_Mouse.Right);
      Buttons (Middle) := btns (PC_Mouse.Middle);
   end Get_State;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      MouseReset;
   end Initialize;

   ------------------
   -- Update_State --
   ------------------

   function Update_State return Boolean is
   begin
      return True;
   end Update_State;
end Mouse;

