with Interfaces.C;

package body Graphics is -- linux
   use Interfaces.C;

   function gl_allocatecontext return GraphicsContext_Pointer;
   pragma Import (C, gl_allocatecontext);

   procedure gl_copyboxtocontext (
     x1, y1 : int;
     w,  h  : int;
     gc     : GraphicsContext_Pointer;
     x2, y2 : int
   );
   pragma Import (C, gl_copyboxtocontext);

   procedure gl_copyscreen (gc : GraphicsContext_Pointer);
   pragma Import (C, gl_copyscreen);

   procedure gl_freecontext (gc : GraphicsContext_Pointer);
   pragma Import (C, gl_freecontext);

   function vga_init return int;
   pragma Import (C, vga_init);

   function vga_setmode (mode : int) return int;
   pragma Import (C, vga_setmode);

   -----------------------------
   -- Copy_Graphics_To_Screen --
   -----------------------------

   procedure Copy_Graphics_To_Screen is
   begin
      if Physical_Screen /= null then
         gl_copyscreen (Physical_Screen);
      else
         raise Graphics_Error;
      end if;
   end Copy_Graphics_To_Screen;

   procedure Copy_Graphics_To_Screen (
     X1, Y1 : Integer;
     X2, Y2 : Integer
   ) is
   begin
      if Physical_Screen /= null then
         gl_copyboxtocontext (
           x1 => int (X1),
           y1 => int (Y1),
           w  => int (X2 - X1),
           h  => int (Y2 - Y1),
           gc => Physical_Screen,
           x2 => int (X1),
           y2 => int (Y1)
         );
      else
         raise Graphics_Error;
      end if;
   end Copy_Graphics_To_Screen;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      if Buffer /= null then
         gl_freecontext (Buffer);
         Buffer := null;
      end if;

      if Physical_Screen /= null then
         -- Should we be deallocating a non-virtual context?
         gl_freecontext (Physical_Screen);
         Physical_Screen := null;
      end if;

      if vga_setmode (0) /= 0 then
         raise Graphics_Error;
      end if;
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      if vga_init /= 0 then
         raise Graphics_Error;
      end if;

      if Buffer = null then
         Buffer := gl_allocatecontext;
      end if;

      if Physical_Screen = null then
         Physical_Screen := gl_allocatecontext;
      end if;
   end Initialize;
end Graphics;
