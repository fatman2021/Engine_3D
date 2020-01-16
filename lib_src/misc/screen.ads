with Ada.Unchecked_Deallocation, Ada.Finalization;
with Graphics.Colors;

package Screen is
   type Byte is mod 2 ** 8;
   for Byte'Size use 8;

   Font_Width  : constant Positive :=  8;
   Font_Height : constant Positive := 14;

   type Data_Buffer is array (Natural range <>) of Graphics.Colors.Color_Index;
private
   for Data_Buffer'Component_Size use Graphics.Colors.Color_Index'Size;
   type Data_Buffer_Access is access all Data_Buffer;

   procedure Free is
     new Ada.Unchecked_Deallocation (Data_Buffer, Data_Buffer_Access);
end Screen;
