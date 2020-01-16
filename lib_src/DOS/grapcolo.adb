package body Graphics.Colors is -- DOS
   -----------------
   -- Get_Palette --
   -----------------

   function Get_Palette return Palette_Type is
   begin
      return Palette_Type (SVGA.Get_Palette);
   end Get_Palette;

   -----------------
   -- Set_Palette --
   -----------------

   procedure Set_Palette (Palette : Palette_Type) is
   begin
      SVGA.Set_Palette (SVGA.Color_Palette (Palette));
   end Set_Palette;

   --------------
   -- To_Color --
   --------------

   function To_Color (RGB_Color : RGB_Color_Type) return Color_Type is
   begin
      return (
        Red   => SVGA.Color_Value (RGB_Color (Red)),
        Green => SVGA.Color_Value (RGB_Color (Green)),
        Blue  => SVGA.Color_Value (RGB_Color (Blue))
      );
   end To_Color;

   ------------------
   -- To_RGB_Color --
   ------------------

   function To_RGB_Color (Color : Color_Type) return RGB_Color_Type is
   begin
      return (
        Red   => Intensity_Type (Color.Red),
        Green => Intensity_Type (Color.Green),
        Blue  => Intensity_Type (Color.Blue)
      );
   end To_RGB_Color;

end Graphics.Colors;

