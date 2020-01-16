with Interfaces.C.Strings;

package body Graphics.Modes is -- linux
   use Interfaces.C;

   type vga_modeinfo is record
      width             : int;
      height            : int;
      bytesperpixel     : int;
      colors            : int;
      linewidth         : int; -- scanline width in bytes
      maxlogicalwidth   : int; -- maximum logical scanline width
      startaddressrange : int; -- changeable bits set
      maxpixels         : int; -- video memory / bytesperpixel
      haveblit          : int; -- mask of blit functions available
      flags             : int; -- other flags
   end record;
   pragma Convention (C, vga_modeinfo);

   type vga_modeinfo_Pointer is access vga_modeinfo;
   pragma Convention (C, vga_modeinfo_Pointer);

   procedure gl_getcontext (gc : GraphicsContext_Pointer);
   pragma Import (C, gl_getcontext);

   function gl_setcontextvga (mode : int) return int;
   pragma Import (C, gl_setcontextvga);

   function gl_setcontextvgavirtual (mode : int) return int;
   pragma Import (C, gl_setcontextvgavirtual);

   function vga_getmodeinfo (mode : int) return vga_modeinfo_Pointer;
   pragma Import (C, vga_getmodeinfo);

   function vga_getmodename (mode : int) return chars_ptr;
   pragma Import (C, vga_getmodename);

   function vga_hasmode (mode : int) return int;
   pragma Import (C, vga_hasmode);

   function vga_lastmodenumber return int;
   pragma Import (C, vga_lastmodenumber);

   function vga_setmode (mode : int) return int;
   pragma Import (C, vga_setmode);

   ---------------------
   -- Available_Modes --
   ---------------------

   function Available_Modes return Mode_Array is
      Count : Natural := 0;
      Info  : vga_modeinfo_Pointer;
      Last_Mode      : constant Mode_Type := Mode_Type (vga_lastmodenumber);
      Mode_Available : array (Mode_Type range 1 .. Last_Mode)
        of Boolean := (others => False);
   begin
      for Mode in Mode_Available'Range loop
         if vga_hasmode (int (Mode)) /= 0 then
            Info := vga_getmodeinfo (int (Mode));
            if Info.bytesperpixel = 1 and Info.colors = 256 then
               Mode_Available (Mode) := True;
               Count := Count + 1;
            end if;
         end if;
      end loop;

      declare
         Mode_List : Mode_Array (1 .. Count);
      begin
         for Mode in reverse Mode_Available'Range loop
            if Mode_Available (Mode) then
               Mode_List (Count) := Mode;
               Count := Count - 1;
            end if;
         end loop;

         return Mode_List;
      end;
   end Available_Modes;

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height return Positive is
   begin
      if Buffer /= null then
         return Positive (Buffer.height);
      else
         raise Graphics_Error;
      end if;
   end Get_Height;

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height (Mode : Mode_Type) return Positive is
      Info : constant vga_modeinfo_Pointer := vga_getmodeinfo (int (Mode));
   begin
      if Info /= null then
         return Positive (Info.height);
      else
         raise Graphics_Error;
      end if;
   end Get_Height;

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width return Positive is
   begin
      if Buffer /= null then
         return Positive (Buffer.width);
      else
         raise Graphics_Error;
      end if;
   end Get_Width;

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width (Mode : Mode_Type) return Positive is
      Info : constant vga_modeinfo_Pointer := vga_getmodeinfo (int (Mode));
   begin
      if Info /= null then
         return Positive (Info.width);
      else
         raise Graphics_Error;
      end if;
   end Get_Width;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode (Mode : Mode_Type) is
   begin
      if (Buffer /= null and Physical_Screen /= null) and then
        vga_setmode (int (Mode)) = 0 and then
          gl_setcontextvga (int (Mode)) = 0 then
         gl_getcontext (Physical_Screen);
         if gl_setcontextvgavirtual (int (Mode)) = 0 then
            gl_getcontext (Buffer);
            return;
         end if;
      end if;

      raise Graphics_Error;
   end Set_Mode;

   ---------------
   -- Text_Mode --
   ---------------

   procedure Text_Mode is
   begin
      if vga_setmode (0) /= 0 then
         raise Graphics_Error;
      end if;
   end Text_Mode;

   ---------------
   -- To_String --
   ---------------

   function To_String (Mode : Mode_Type) return String is
      Mode_Name : constant chars_ptr := vga_getmodename (int (Mode));
   begin
      if Mode_Name /= Null_Ptr then
         return Strings.Value (Mode_Name);
      else
         raise Graphics_Error;
      end if;
   end To_String;
end Graphics.Modes;
