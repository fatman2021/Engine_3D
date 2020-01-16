package Graphics is -- DOS
   Graphics_Error : exception;

   procedure Initialize;
   procedure Finalize;

   --  Copy the buffer to the screen
   procedure Copy_Graphics_To_Screen;
   pragma Inline (Copy_Graphics_To_Screen);

   --  Copy a rectangular part of the buffer to the screen
   procedure Copy_Graphics_To_Screen (
     X1, Y1 : Integer;
     X2, Y2 : Integer
   );
   pragma Inline (Copy_Graphics_To_Screen);
private
   procedure Set_Text_Mode;

   Byte_Size : constant := 8;
   type Byte_Type is mod 2 ** Byte_Size;
   for Byte_Type'Size use Byte_Size;
   pragma Convention (C, Byte_Type);

   type Buffer_Type is array (Natural range <>, Natural range <>) of
     Byte_Type;
   for Buffer_Type'Component_Size use Byte_Size;
   pragma Convention (C, Buffer_Type);

   type Buffer_Pointer is access Buffer_Type;

   Buffer : Buffer_Pointer;

   X_Size, Y_Size, Count : Positive; -- Count = X_Size * Y_Size
end Graphics;
