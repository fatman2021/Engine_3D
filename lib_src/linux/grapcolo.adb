package body Graphics.Colors is -- linux
   use Interfaces.C;

   procedure gl_getpalettecolors (
     s  : in     int;
     n  : in     int;
     sp :    out Palette_Type
   );
   pragma Import (C, gl_getpalettecolors);

   procedure gl_setpalettecolors (
     s  : in     int;
     n  : in     int;
     sp : in     Palette_Type
   );
   pragma Import (C, gl_setpalettecolors);

   -----------------
   -- Get_Palette --
   -----------------

   procedure Get_Palette (Palette : out Palette_Type) is
   begin
      gl_getpalettecolors (int (Palette'First), Palette'Length, Palette);
   end Get_Palette;

   -----------------
   -- Set_Palette --
   -----------------

   procedure Set_Palette (Palette : Palette_Type) is
   begin
      gl_setpalettecolors (int (Palette'First), Palette'Length, Palette);
   end Set_Palette;

   -------------
   -- To_Byte --
   -------------

   function To_Byte (Intensity : Intensity_Type) return Byte_Type;

   function To_Byte (Intensity : Intensity_Type) return Byte_Type is
   begin
      return Byte_Type (Intensity / Intensity_Type'Delta);
   end To_Byte;

   ------------------
   -- To_Intensity --
   ------------------

   function To_Intensity (Byte : Byte_Type) return Intensity_Type;

   function To_Intensity (Byte : Byte_Type) return Intensity_Type is
   begin
      return Integer (Byte) * Intensity_Type'Delta;
   end To_Intensity;

   --------------
   -- To_Color --
   --------------

   function To_Color (RGB_Color : RGB_Color_Type) return Color_Type is
   begin
      return (
        Red   => To_Byte (RGB_Color (Red)),
        Green => To_Byte (RGB_Color (Green)),
        Blue  => To_Byte (RGB_Color (Blue))
      );
   end To_Color;

   ------------------
   -- To_RGB_Color --
   ------------------

   function To_RGB_Color (Color : Color_Type) return RGB_Color_Type is
   begin
      return (
        Red   => To_Intensity (Color (Red)),
        Green => To_Intensity (Color (Green)),
        Blue  => To_Intensity (Color (Blue))
      );
   end To_RGB_Color;
end Graphics.Colors;

