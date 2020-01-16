with Graphics.Colors;

package Graphics.Buffers is -- DOS
   pragma Elaborate_Body;

   type Buffer_Type is array (Integer range <>, Integer range <>)
     of Graphics.Colors.Color_Index;

   type Buffer_Pointer is access Buffer_Type;

   procedure Get_Buffer (
     Buffer   :    out Buffer_Type;
     X_Offset : in     Integer := 0;
     Y_Offset : in     Integer := 0
   );
   pragma Inline (Get_Buffer);

   procedure Put_Buffer (
     Buffer   : in     Buffer_Type;
     X_Offset : in     Integer := 0;
     Y_Offset : in     Integer := 0
   );
   pragma Inline (Put_Buffer);
private
   for Buffer_Type'Component_Size use Graphics.Colors.Color_Index'Size;
end Graphics.Buffers;
