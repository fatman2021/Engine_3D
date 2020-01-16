with Interfaces.C;

package body Graphics.Buffers is -- linux
   use Interfaces.C;

   procedure gl_getbox (
     x, y : in     int;
     w, h : in     int;
     dp   :    out Linear_Buffer
   );

   procedure gl_getbox (
     x, y : in     int;
     w, h : in     int;
     dp   :    out Rectangular_Buffer
   );
   pragma Import (C, gl_getbox);

   procedure gl_putbox (
     x, y : in     int;
     w, h : in     int;
     dp   : in     Linear_Buffer
   );

   procedure gl_putbox (
     x, y : in     int;
     w, h : in     int;
     dp   : in     Rectangular_Buffer
   );
   pragma Import (C, gl_putbox);

   ----------------
   -- Get_Buffer --
   ----------------

   procedure Get_Buffer (
     Buffer   :    out Linear_Buffer;
     Y        : in     Integer;
     X_Offset : in     Integer := 0
   ) is
   begin
      gl_getbox (
        x  => int (Buffer'First + X_Offset),
        y  => int (Y),
        w  => Buffer'Length,
        h  => 1,
        dp => Buffer
      );
   end Get_Buffer;

   procedure Get_Buffer (
     Buffer   :    out Rectangular_Buffer;
     X_Offset : in     Integer := 0;
     Y_Offset : in     Integer := 0
   ) is
   begin
      gl_getbox (
        x  => int (Buffer'First (2) + X_Offset),
        y  => int (Buffer'First (1) + Y_Offset),
        w  => Buffer'Length (2),
        h  => Buffer'Length (1),
        dp => Buffer
      );
   end Get_Buffer;

   ----------------
   -- Put_Buffer --
   ----------------

   procedure Put_Buffer (
     Buffer   : in     Linear_Buffer;
     Y        : in     Integer;
     X_Offset : in     Integer := 0
   ) is
   begin
      gl_putbox (
        x  => int (Buffer'First + X_Offset),
        y  => int (Y),
        w  => Buffer'Length,
        h  => 1,
        dp => Buffer
      );
   end Put_Buffer;

   procedure Put_Buffer (
     Buffer   : in     Rectangular_Buffer;
     X_Offset : in     Integer := 0;
     Y_Offset : in     Integer := 0
   ) is
   begin
      gl_putbox (
        x  => int (Buffer'First (2) + X_Offset),
        y  => int (Buffer'First (1) + Y_Offset),
        w  => Buffer'Length (2),
        h  => Buffer'Length (1),
        dp => Buffer
      );
   end Put_Buffer;
end Graphics.Buffers;

