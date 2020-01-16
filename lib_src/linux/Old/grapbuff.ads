with Graphics.Colors;

package Graphics.Buffers is -- linux
   pragma Elaborate_Body;

   type Linear_Buffer is array (Integer range <>)
     of Graphics.Colors.Color_Index;

   --  First coordinate is Y value, second coordinate is X value
   type Rectangular_Buffer is array (Integer range <>, Integer range <>)
     of Graphics.Colors.Color_Index;

   procedure Get_Buffer (
     Buffer   :    out Linear_Buffer;
     Y        : in     Integer;
     X_Offset : in     Integer := 0
   );
   pragma Inline (Get_Buffer);

   procedure Get_Buffer (
     Buffer   :    out Rectangular_Buffer;
     X_Offset : in     Integer := 0;
     Y_Offset : in     Integer := 0
   );
   pragma Inline (Get_Buffer);

   procedure Put_Buffer (
     Buffer   : in     Linear_Buffer;
     Y        : in     Integer;
     X_Offset : in     Integer := 0
   );
   pragma Inline (Put_Buffer);

   procedure Put_Buffer (
     Buffer   : in     Rectangular_Buffer;
     X_Offset : in     Integer := 0;
     Y_Offset : in     Integer := 0
   );
   pragma Inline (Put_Buffer);
private
   for Linear_Buffer'Component_Size use Graphics.Colors.Color_Index'Size;
   for Rectangular_Buffer'Component_Size use Graphics.Colors.Color_Index'Size;
end Graphics.Buffers;
