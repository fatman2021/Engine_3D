with Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Graphics.Internals;
with Interfaces;

package body Graphics is -- DOS
   use Graphics.Internals;
   use Interfaces;

   --------------------------------------
   -- Maximum size of a VBE info block --
   --------------------------------------
   Max_Infoblock_Length : constant := 512;

   -------------------
   -- VBE Constants --
   -------------------
   Window_Active       : constant unsigned_8  := 16#07#;
   Window_A            : constant unsigned_16 := 16#0000#;
   Mode_Supported      : constant unsigned_16 := 16#0001#;
   Linear_Mode_Present : constant unsigned_16 := 16#0080#;
   VBE_Call_Ok         : constant unsigned_16 := 16#004F#;
   Linear_Mode_Flag    : constant unsigned_16 := 16#4000#;
   VBE_Signature       : constant unsigned_32 := 16#41534556#;
   VBE_All_States      : constant unsigned_16 := 16#000F#;
   Video_Interrupt     : constant unsigned_16 := 16#0010#;
   Text_Address        : constant unsigned_32 := 16#B8000#;

   ----------------------------
   -- VBE Driver information --
   ----------------------------
   type VbeInfoBlock is
      record
         VBESignature : Unsigned_32; -- 4 signature bytes
         VBEVersion   : Unsigned_16; -- VBE version number
         OEMStringPtr : Unsigned_32; -- Pointer to OEM string
         Capabilities : Unsigned_32; -- capabilities of the video environment
         VideoModePtr : Unsigned_32; -- pointer to supported Super VGA modes
         TotalMemory  : Unsigned_16; -- Number of 64kb memory blocks on board
      end record;
   pragma Pack (VbeInfoBlock);

   ----------------------
   -- VBE State Buffer --
   ----------------------
   type VBE_State is array (Natural range <>) of Unsigned_8;
   pragma Convention (C, VBE_State);

   type State_Access is access all VBE_State;
   procedure Free is new Ada.Unchecked_Deallocation (VBE_State, State_Access);

   Graphics_Initialized  : Boolean := False;
   Supports_State_Buffer : Boolean := True; -- ATI MACH64 doesn't

   Startup_Mode : Unsigned_16;
   State_Buffer : State_Access;

   ------------------------
   -- VBE function calls --
   ------------------------

   ------------------
   -- GetStateSize --
   ------------------
   function GetStateSize return Natural is
      Regs : Dpmi_Regs;
   begin
      Regs.Ax := 16#4F04#;
      Regs.Dx := 0;
      Regs.CX := VBE_All_States;
      Dpmi_Int (Video_Interrupt, Regs);
      if Regs.Ax /= VBE_Call_Ok then
         Ada.Exceptions.Raise_Exception (
           Graphics_Error'Identity,
           "VBE error"
         );
      end if;
      return Natural (Regs.Bx) * 64;
   end GetStateSize;
   pragma Inline (GetStateSize);

   ----------------
   -- GetVBEMode --
   ----------------
   function GetVBEMode return Unsigned_16 is
      Regs : Dpmi_Regs;
   begin
      Regs.Ax := 16#4F03#;
      Dpmi_Int (Video_Interrupt, Regs);
      if Regs.Ax /= VBE_Call_Ok then
         Ada.Exceptions.Raise_Exception (
           Graphics_Error'Identity,
           "VBE error"
         );
      end if;
      return Regs.Bx;
   end GetVBEMode;
   pragma Inline (GetVBEMode);

   ----------------
   -- SetVBEMode --
   ----------------
   procedure SetVBEMode (Mode : in Unsigned_16) is
      Regs : Dpmi_Regs;
   begin
      Regs.Ax := 16#4F02#;
      Regs.Bx := Mode;
      Dpmi_Int (Video_Interrupt, Regs);
      if Regs.Ax /= VBE_Call_Ok then
         Ada.Exceptions.Raise_Exception (
           Graphics_Error'Identity,
           "VBE error"
         );
      end if;
   end SetVBEMode;
   pragma Inline (SetVBEMode);

   ---------------
   -- SaveState --
   ---------------
   procedure SaveState (Buffer : out VBE_State) is
      Regs : Dpmi_Regs;
   begin
      if Buffer'Size = 0 then
         Ada.Exceptions.Raise_Exception (
           Graphics_Error'Identity,
           "VBE error"
         );
      end if;
      if (Buffer'Size / 8) >= TB_Size then
         Ada.Exceptions.Raise_Exception (
           Graphics_Error'Identity,
           "VBE error"
         );
      end if;
      Regs.Ax := 16#4F04#;
      Regs.Dx := 1;
      Regs.CX := VBE_All_States;
      Regs.Es := RM_Segment (TB_Address);
      Regs.Bx := RM_Offset  (TB_Address);
      Dpmi_Int (Video_Interrupt, Regs);
      if Regs.Ax /= VBE_Call_Ok then
         Ada.Exceptions.Raise_Exception (
           Graphics_Error'Identity,
           "VBE error"
         );
      end if;
      Dosmemget (TB_Address, Buffer'Size / 8, Buffer'Address);
   end SaveState;
   pragma Inline (SaveState);

   ------------------
   -- RestoreState --
   ------------------
   procedure RestoreState (Buffer : in VBE_State) is
      Regs : Dpmi_Regs;
   begin
      if Buffer'Size = 0 then
         Ada.Exceptions.Raise_Exception (
           Graphics_Error'Identity,
           "VBE error"
         );
      end if;
      if (Buffer'Size / 8) >= TB_Size then
         Ada.Exceptions.Raise_Exception (
           Graphics_Error'Identity,
           "VBE error"
         );
      end if;
      Dosmemput (Buffer'Address, Buffer'Size / 8, TB_Address);
      Regs.Ax := 16#4F04#;
      Regs.Dx := 2;
      Regs.CX := VBE_All_States;
      Regs.Es := RM_Segment (TB_Address);
      Regs.Bx := RM_Offset  (TB_Address);
      Dpmi_Int (Video_Interrupt, Regs);
      if Regs.Ax /= VBE_Call_Ok then
         Ada.Exceptions.Raise_Exception (
           Graphics_Error'Identity,
           "VBE error"
         );
      end if;
   end RestoreState;
   pragma Inline (RestoreState);

   ---------------------
   -- GetVBEInfoBlock --
   ---------------------
   procedure GetVBEInfoBlock (Info : in out VbeInfoBlock) is
      Regs : Dpmi_Regs;
   begin
      if TB_Size < Max_Infoblock_Length then
         Ada.Exceptions.Raise_Exception (
           Graphics_Error'Identity,
           "VBE error"
         );
      end if;
      Regs.Ax := 16#4F00#;
      Regs.Es := RM_Segment (TB_Address);
      Regs.Di := RM_Offset (TB_Address);
      Dpmi_Int (Video_Interrupt, Regs);
      if Regs.Ax /= VBE_Call_Ok then
         Ada.Exceptions.Raise_Exception (
           Graphics_Error'Identity,
           "VBE error"
         );
      end if;
      Dosmemget (TB_Address, Info'Size / 8, Info'Address);
   end GetVBEInfoBlock;
   pragma Inline (GetVBEInfoBlock);

   ---------------------
   -- Initialize_Vesa --
   ---------------------
   procedure Initialize_Vesa is
      Info : VbeInfoBlock;
   begin
      begin
        State_Buffer := new VBE_State (0 .. GetStateSize - 1);
        SaveState (State_Buffer.all);
        Supports_State_Buffer:= True;
      exception
        when Graphics_Error => Supports_State_Buffer:= False;
      end;
      Startup_Mode := GetVBEMode;
      Info.VBESignature := 16#45325642#;  -- 'VBE2'
      GetVBEInfoBlock (Info);
      if Info.VBESignature /= VBE_Signature then
         Ada.Exceptions.Raise_Exception (
           Graphics_Error'Identity,
           "No VBE driver"
         );
      end if;
      case Info.VBEVersion is
         when 16#0102#|16#010A# => Linear_Mode := False;
         when 16#0200#|16#0201# => Linear_Mode := True;
         when 16#0300# => Linear_Mode := True;
         when others   =>
            Ada.Exceptions.Raise_Exception (
              Graphics_Error'Identity,
              "Invalid VBE version"
            );
      end case;
--      Load_Video_Modes (Info, Available_Modes);
   end Initialize_Vesa;

   -----------------------------
   -- Copy_Graphics_To_Screen --
   -----------------------------
   procedure Copy_Graphics_To_Screen is
   begin
      null; --QQ!
   end Copy_Graphics_To_Screen;

   procedure Copy_Graphics_To_Screen (
     X1, Y1 : Integer;
     X2, Y2 : Integer
   ) is
   begin
      null; --QQ not yet implemented
   end Copy_Graphics_To_Screen;

   --------------
   -- Finalize --
   --------------
   procedure Finalize is
   begin
      if Graphics_Initialized then
         Set_Text_Mode;
         Graphics_Initialized := False;
      end if;
   end Finalize;

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize is
   begin
      if Graphics_Initialized then
         Ada.Exceptions.Raise_Exception (
           Graphics_Error'Identity,
           "Already initialized"
         );
      else
         Prog_Selector  := My_Ds;
         Video_Selector := Unsigned_32 (DOS_Ds);
         Set_Selector (DOS_Ds);
   
         Initialize_Vesa;
   
--         Default_Palette := Get_Palette;
--         Load_System_Font (System_Font);
      end if;
   end Initialize;

   -------------------
   -- Set_Text_Mode --
   -------------------
   procedure Set_Text_Mode is
      Regs : Dpmi_Regs;
   begin
      if not In_Graphics_Mode then
         return;
      end if;

      if Linear_Mode then
         if Dpmi_Free_Ldt_Descriptor (Video_Selector) = -1 then
            Ada.Exceptions.Raise_Exception (
              Graphics_Error'Identity,
              "Cannot free local desc"
            );
         end if;
      end if;

      if (Startup_Mode and 16#FF00#) = 0 then
         Regs.ax := Startup_Mode;
         Dpmi_Int (Video_Interrupt, Regs);
      else
         SetVBEMode (Startup_Mode);
      end if;

      if Supports_State_Buffer then
         RestoreState (State_Buffer.all);
         Free (State_Buffer);
      end if;

--QQ      Restore_Default_Palette;

      if Text_Rows = 43 then
         Regs.Ax := 16#1201#;
         Regs.Bx := 16#0030#;
         Dpmi_Int (Video_Interrupt, Regs);
      elsif Text_Rows = 50 then
         Regs.Ax := 16#1202#;
         Regs.Bx := 16#0030#;
         Dpmi_Int (Video_Interrupt, Regs);
      end if;

      if Text_Rows = 43 or Text_Rows = 50 then
         Regs.Ax := 16#1112#;
         Regs.Bx := 0;
         Dpmi_Int (Video_Interrupt, Regs);
      end if;

      Regs.Ax := 16#0500# or Text_Page;
      Dpmi_Int (Video_Interrupt, Regs);

      Regs.Ax := 16#0200#;
      Regs.Bx := Shift_Left (Text_Page, 8);
      Regs.Dx := Text_Cursor;
      Dpmi_Int (Video_Interrupt, Regs);

      Dosmemput (Text_Buffer'Address, Text_Size, Text_Address);

      In_Graphics_Mode := False;
   end Set_Text_Mode;
end Graphics;
