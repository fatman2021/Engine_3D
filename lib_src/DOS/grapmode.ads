package Graphics.Modes is -- DOS
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
   type Mode_Type is (M640x400, M640x480, M800x600,
                      M1024x768, M1280x1024, M1600x1200
                     );
   for Mode_Type use (M640x400 => 16#0100#, M640x480 => 16#0101#,
                      M800x600 => 16#0103#, M1024x768 => 16#0105#,
                      M1280x1024 => 16#0107#, M1600x1200 => 16#0120#
                     );
   for Mode_Type'Size use 16;
end Graphics.Modes;
