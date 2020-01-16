with Ada.Unchecked_Conversion;
with Interfaces.C;

package body Graphics.Drawing is -- linux
   use Interfaces.C;

   procedure gl_circle (x, y, r, c : int);
   pragma Import (C, gl_circle);

   procedure gl_clearscreen (c : int);
   pragma Import (C, gl_clearscreen);

   function gl_getpixel (x, y : int) return int;
   pragma Import (C, gl_getpixel);

   procedure gl_fillbox (x, y, w, h, c : int);
   pragma Import (C, gl_fillbox);

   procedure gl_hline (x1, y, x2, c : int);
   pragma Import (C, gl_hline);

   procedure gl_line (x1, y1, x2, y2, c : int);
   pragma Import (C, gl_line);

   procedure gl_setpixel (x, y, c : int);
   pragma Import (C, gl_setpixel);

   ------------
   -- Circle --
   ------------

   procedure Circle (
     X, Y   : Integer;
     Radius : Natural;
     Index  : Colors.Color_Index
   ) is
   begin
      gl_circle (int (X), int (Y), int (Radius), int (Index));
   end Circle;

   --------------------
   -- Clear_Graphics --
   --------------------

   procedure Clear_Graphics (Index : Colors.Color_Index) is
   begin
      gl_clearscreen (int (Index));
   end Clear_Graphics;

   --------------------
   -- Fill_Rectangle --
   --------------------

   procedure Fill_Rectangle (
     X, Y   : Integer;
     Width  : Natural;
     Height : Natural;
     Index  : Colors.Color_Index
   ) is
   begin
      gl_fillbox (int (X), int (Y), int (Width), int (Height), int (Index));
   end Fill_Rectangle;

   ---------------
   -- Get_Pixel --
   ---------------

   function Get_Pixel (X, Y : Integer) return Colors.Color_Index is
      pixel : int := gl_getpixel (int (X), int (Y));
   begin
      return Colors.Color_Index (gl_getpixel (int (X), int (Y)));
   exception
      when Constraint_Error =>
         raise Graphics_Error;
   end Get_Pixel;

   ---------------------
   -- Horizontal_Line --
   ---------------------

   procedure Horizontal_Line (
     X1, X2 : Integer;
     Y      : Integer;
     Index  : Colors.Color_Index
   ) is
   begin
      gl_hline (int (X1), int (Y), int (X2), int (Index));
   end Horizontal_Line;

   ----------
   -- Line --
   ----------

   procedure Line (
     X1, Y1 : Integer;
     X2, Y2 : Integer;
     Index  : Colors.Color_Index
   ) is
   begin
      gl_line (int (X1), int (Y1), int (X2), int (Y2), int (Index));
   end Line;

   ---------------
   -- Set_Pixel --
   ---------------

   procedure Set_Pixel (
     X, Y  : Integer;
     Index : Colors.Color_Index
   ) is
   begin
      gl_setpixel (int (X), int (Y), int (Index));
   end Set_Pixel;

end Graphics.Drawing;

