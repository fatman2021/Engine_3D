with Interfaces.C.Strings;

package Graphics is -- linux
   pragma Elaborate_Body;

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
   use Interfaces.C;
   use Interfaces.C.Strings;

   type GraphicsContext is record
      modetype      : unsigned_char; -- virtual, paged, linear, mode X
      modeflags     : unsigned_char; -- or planar16
      dummy         : unsigned_char;
      flippage      : unsigned_char;
      width         : int;           -- width in pixels
      height        : int;           -- height in pixels
      bytesperpixel : int;           -- bytes per pixel (1, 2, 3, or 4)
      colors        : int;           -- number of colors
      bitsperpixel  : int;           -- bits per pixel (8, 15, 16 or 24)
      bytewidth     : int;           -- length of a scanline in bytes
      vbuf          : chars_ptr;     -- address of framebuffer
      clip          : int;           -- clipping enabled?
      clipx1        : int;           -- top-left coordinate of clip window
      clipy1        : int;
      clipx2        : int;           -- bottom-right coordinate of clip window
      clipy2        : int;
   end record;
   pragma Convention (C, GraphicsContext);

   type GraphicsContext_Pointer is access GraphicsContext;
   pragma Convention (C, GraphicsContext_Pointer);

   Buffer, Physical_Screen : GraphicsContext_Pointer;
end Graphics;
