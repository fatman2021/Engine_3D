package Mouse is
   pragma Elaborate_Body;

   Mouse_Error : exception;

   --  Raises Mouse_Error if no mouse installed
   procedure Initialize;
   pragma Inline (Initialize);

   procedure Finalize;
   pragma Inline (Finalize);

   type Button_Type is (Left, Right, Middle);

   type Press_Array is array (Button_Type) of Boolean;

   --  Returns True if a new mouse event has occurred
   function Update_State return Boolean;
   pragma Inline (Update_State);

   procedure Get_State (
     X, Y    : out Integer;
     Buttons : out Press_Array
   );
   pragma Inline (Get_State);
end Mouse;
