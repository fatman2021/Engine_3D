package Graphics.Colors is -- DOS
   pragma Elaborate_Body;

   Max_Colors : constant := 256;

   type Color_Type is private;

   type Color_Index is range 0 .. Max_Colors - 1;

   type Palette_Type is array (Color_Index range <>) of Color_Type;

   type Intensity_Type is delta 1.0/63.0 range 0.0 .. 1.0;
   type Component_Type is (Red, Green, Blue);
   type RGB_Color_Type is array (Component_Type) of Intensity_Type;

   procedure Get_Palette (Palette :    out Palette_Type);
   procedure Set_Palette (Palette : in     Palette_Type);

   function To_Color (RGB_Color : RGB_Color_Type) return Color_Type;

   function To_RGB_Color (Color : Color_Type) return RGB_Color_Type;
private
   Byte_Size : constant := 8;
   type Byte_Type is mod 2 ** Byte_Size;
   for Byte_Type'Size use Byte_Size;
   pragma Convention (C, Byte_Type);

   type Color_Type is array (Component_Type) of Byte_Type;
   for Color_Type'Component_Size use Byte_Size;

   for Color_Index'Size use Byte_Size;

   for Palette_Type'Component_Size use 3 * Byte_Size;
   pragma Convention (C, Palette_Type);
end Graphics.Colors;
