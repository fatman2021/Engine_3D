with Graphics.Colors;

package Graphics.Drawing is
   pragma Elaborate_Body;

   procedure Circle (
     X, Y   : Integer;
     Radius : Natural;
     Index  : Colors.Color_Index
   );
   pragma Inline (Circle);

   procedure Clear_Graphics (Index : Colors.Color_Index);
   pragma Inline (Clear_Graphics);

   function Get_Pixel (X, Y : Integer) return Colors.Color_Index;
   pragma Inline (Get_Pixel);

   procedure Fill_Rectangle (
     X, Y   : Integer;
     Width  : Natural;
     Height : Natural;
     Index  : Colors.Color_Index
   );
   pragma Inline (Fill_Rectangle);

   procedure Horizontal_Line (
     X1, X2 : Integer;
     Y      : Integer;
     Index  : Colors.Color_Index
   );
   pragma Inline (Horizontal_Line);

   procedure Line (
     X1, Y1 : Integer;
     X2, Y2 : Integer;
     Index  : Colors.Color_Index
   );
   pragma Inline (Line);

   procedure Set_Pixel (
     X, Y  : Integer;
     Index : Colors.Color_Index
   );
   pragma Inline (Set_Pixel);
end Graphics.Drawing;
