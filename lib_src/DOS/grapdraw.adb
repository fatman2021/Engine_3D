with Graphics.Internals;

package body Graphics.Drawing is -- DOS
   use Graphics.Internals;

   ---------------
   -- Set_Pixel --
   ---------------
   procedure Set_Pixel (
     X, Y  : Integer;
     Index : Colors.Color_Index
   ) is
   begin
      if X >= 0 and Y >= 0 and X < X_Size and Y < Y_Size then
         Buffer (X, Y) := Byte_Type (Index);
      end if;
   end Set_Pixel;

   ------------
   -- Circle --
   ------------
   procedure Circle (
     X, Y   : Integer;
     Radius : Natural;
     Index  : Colors.Color_Index
   ) is
   begin
      null; --QQ
   end Circle;

   --------------------
   -- Clear_Graphics --
   --------------------
   procedure Clear_Graphics (Index : Colors.Color_Index) is
   begin
      Mem_Set (Buffer.all'Address, Integer (Index), Count);
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
      null; --QQ
   end Fill_Rectangle;

   ---------------
   -- Get_Pixel --
   ---------------
   function Get_Pixel (X, Y : Integer) return Colors.Color_Index is
   begin
      if X >= 0 and Y >= 0 and X < X_Size and Y < Y_Size then
         return Colors.Color_Index (Buffer (X, Y));
      else
         raise Graphics_Error;
      end if;
   end Get_Pixel;

   ---------------------
   -- Horizontal_Line --
   ---------------------
   procedure Horizontal_Line (
     X1, X2 : Integer;
     Y      : Integer;
     Index  : Colors.Color_Index
   ) is
      Left, Right : Integer;
   begin
      if X1 = X2 then
         Set_Pixel (X1, Y, Index);
      else
         --QQ Do range checking!
         Left  := X1;
         Right := X2;

         if X1 > X2 then
            Left  := X2;
            Right := X1;
         end if;

         Mem_Set (Buffer (Left, Y)'Address, Integer (Index), Right - Left);
      end if;
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
      null; --QQ!
   end Line;
end Graphics.Drawing;

