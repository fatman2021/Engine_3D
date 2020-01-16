with Interfaces;
with System;

private package Graphics.Internals is
   use Interfaces;
   use System;

   Text_Size : constant Unsigned_32 := 2*160*60; -- 2 ** 16;

   -------------------------
   -- Processor registers --
   -------------------------
   type Dpmi_Regs is
      record
         Di     : Unsigned_16;
         Di_Hi  : Unsigned_16;
         Si     : Unsigned_16;
         Si_Hi  : Unsigned_16;
         Bp     : Unsigned_16;
         Bp_Hi  : Unsigned_16;
         Res    : Unsigned_16;
         Res_Hi : Unsigned_16;
         Bx     : Unsigned_16;
         Bx_Hi  : Unsigned_16;
         Dx     : Unsigned_16;
         Dx_Hi  : Unsigned_16;
         Cx     : Unsigned_16;
         Cx_Hi  : Unsigned_16;
         Ax     : Unsigned_16;
         Ax_Hi  : Unsigned_16;
         Flags  : Unsigned_16;
         Es     : Unsigned_16;
         Ds     : Unsigned_16;
         Fs     : Unsigned_16;
         Gs     : Unsigned_16;
         Ip     : Unsigned_16;
         Cs     : Unsigned_16;
         Sp     : Unsigned_16;
         Ss     : Unsigned_16;
      end record;
   pragma Convention (C, Dpmi_Regs);

   ----------------------
   -- Global variables --
   ----------------------
--   Display_Address    : Address;
--   Bank_Size          : Natural;
--   Line_Size          : Natural;
--   Display_Offset     : Natural;
--   System_Font        : Font_Type;
   Text_Page          : Unsigned_16;
   Text_Cursor        : Unsigned_16;
   Text_Rows          : Unsigned_16;
--   Startup_Mode       : Unsigned_16;
--   Current_Bank       : Unsigned_16;
   Prog_Selector      : Unsigned_32;
   VideoModePtr       : Unsigned_32; -- pointer to supported Super VGA modes
   Video_Selector     : Unsigned_32;
--   Default_Palette    : Color_Palette;
   Linear_Mode        : Boolean := False;
   In_Graphics_Mode   : Boolean := False;
--   Available_Modes    : Video_Modes_Array;
   Text_Buffer        : String (1 .. Positive (Text_Size));

   --------------------------------
   -- Call a real-mode interrupt --
   --------------------------------
   procedure Dpmi_Int (Vector : in Unsigned_16; Regs : in out Dpmi_Regs);
   pragma Import (C, Dpmi_Int, "__dpmi_int");

   --------------------------------
   -- Selector for linear memory --
   --------------------------------
   function DOS_Ds return Unsigned_16;
   pragma Inline (DOS_Ds);

   procedure Dosmemget (Offset : in Unsigned_32;
                        Length : in Unsigned_32;
                        Buffer : in Address);
   pragma Import (C, Dosmemget, "dosmemget");

   procedure Dosmemput (Buffer : in Address;
                        Length : in Unsigned_32;
                        Offset : in Unsigned_32);
   pragma Import (C, Dosmemput, "dosmemput");

   function Dpmi_Free_Ldt_Descriptor (Number : Unsigned_32)
     return Integer;
   pragma Import (C, Dpmi_Free_Ldt_Descriptor, "__dpmi_free_ldt_descriptor");

   -----------------
   -- Fill memory --
   -----------------
   procedure Mem_Set (Buffer : Address;
                      Value  : Integer;
                      Number : Integer);
   pragma Import (C, Mem_Set, "memset");

   ----------------------------------
   -- Selector for current program --
   ----------------------------------
   function My_Ds return Unsigned_32;
   pragma Import (C, My_Ds, "_my_ds");

   function Peek_Word (Offset : Integer) return Unsigned_16;

   ---------------------------------------------------------
   -- Convert a DWORD real-mode address into a linear one --
   ---------------------------------------------------------
   function RM_To_Linear (Address : Unsigned_32) return Unsigned_32;
   pragma Inline (RM_To_Linear);

   function RM_Offset (Address : Unsigned_32) return Unsigned_16;
   pragma Inline (RM_Offset);

   function RM_Segment (Address : Unsigned_32) return Unsigned_16;
   pragma Inline (RM_Segment);

   procedure Set_Selector (Selector : in Unsigned_16);
   pragma Import (C, Set_Selector, "_farsetsel");

   ---------------------------------------
   -- Linear address of transfer buffer --
   ---------------------------------------
   function TB_Address return Unsigned_32;
   pragma Inline (TB_Address);

   -----------------------------
   -- Size of transfer buffer --
   -----------------------------
   function TB_Size return Unsigned_32;
   pragma Inline (TB_Size);
end Graphics.Internals;
