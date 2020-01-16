package Keyboard is
   pragma Elaborate_Body;

   Keyboard_Error : exception;

   procedure Initialize;
   pragma Inline (Initialize);

   procedure Finalize;
   pragma Inline (Finalize);

   type Key_Code is private;

   function Is_Pressed (Key : Key_Code) return Boolean;
private
end Keyboard;
