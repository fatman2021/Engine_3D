with Interfaces.C;

package Graphics.Modes is -- linux
   pragma Elaborate_Body;

   type Mode_Type is private;

   type Mode_Array is array (Positive range <>) of Mode_Type;

   function Available_Modes return Mode_Array;

   function Get_Height return Positive; -- Height in pixels of current mode
   pragma Inline (Get_Height);

   function Get_Height (Mode : Mode_Type) return Positive;
   pragma Inline (Get_Height);

   function Get_Width return Positive; -- Width in pixels of current mode
   pragma Inline (Get_Width);

   function Get_Width (Mode : Mode_Type) return Positive;
   pragma Inline (Get_Width);

   procedure Set_Mode (Mode : Mode_Type);

   procedure Text_Mode;

   function To_String (Mode : Mode_Type) return String;
   pragma Inline (To_String);
private
   type Mode_Type is new Interfaces.C.int;
end Graphics.Modes;
