package body Graphics.Internals is
   ------------------------------
   -- DOS extender information --
   ------------------------------
   type Go32_Info_Block is
      record
         Size_Of_This_Structure_In_Bytes       : Unsigned_32;
         Linear_Address_Of_Primary_Screen      : Unsigned_32;
         Linear_Address_Of_Secondary_Screen    : Unsigned_32;
         Linear_Address_Of_Transfer_Buffer     : Unsigned_32;
         Size_Of_Transfer_Buffer               : Unsigned_32;
         Pid                                   : Unsigned_32;
         Master_Interrupt_Controller_Base      : Unsigned_8;
         Slave_Interrupt_Controller_Base       : Unsigned_8;
         Selector_For_Linear_Memory            : Unsigned_16;
         Linear_Address_Of_Stub_Info_Structure : Unsigned_32;
         Linear_Address_Of_Original_Psp        : Unsigned_32;
         Run_Mode                              : Unsigned_16;
         Run_Mode_Info                         : Unsigned_16;
      end record;
   pragma Convention (C, Go32_Info_Block);

   DOS_Extender : Go32_Info_Block;
   pragma Import (C, DOS_Extender, "_go32_info_block");

   ------------
   -- DOS_Ds --
   ------------
   function DOS_Ds return Unsigned_16 is
   begin
      return DOS_Extender.Selector_For_Linear_Memory;
   end DOS_Ds;

   ---------------
   -- Peek_Word --
   ---------------
   function Peek_Word (Offset : Integer) return Unsigned_16 is
      Result : Unsigned_16;
   begin
      ASM (".byte 0x64; movw (%k1), %w0",
        Unsigned_16'Asm_Output ("=r", (result)),
        Integer'Asm_Input ("r", Offset),
        Volatile => True);
      return Result;
   end Peek_Word;

   ------------------
   -- RM_To_Linear --
   ------------------
   function RM_To_Linear (Address : unsigned_32) return unsigned_32 is
   begin
      return 16 * Shift_Right (Address, 16) + (Address and 16#FFFF#);
   end RM_To_Linear;

   ---------------
   -- RM_Offset --
   ---------------
   function RM_Offset (Address : unsigned_32) return unsigned_16 is
   begin
      return unsigned_16 (Address and 16#0F#);
   end RM_Offset;

   ----------------
   -- RM_Segment --
   ----------------
   function RM_Segment (Address : unsigned_32) return unsigned_16 is
   begin
      return unsigned_16 (Shift_Right (Address, 4) and 16#FFFF#);
   end RM_Segment;

   ----------------
   -- TB_Address --
   ----------------
   function TB_Address return Unsigned_32 is
   begin
      return DOS_Extender.Linear_Address_Of_Transfer_Buffer;
   end TB_Address;

   -------------
   -- TB_Size --
   -------------
   function TB_Size return Unsigned_32 is
   begin
      return DOS_Extender.Size_Of_Transfer_Buffer;
   end TB_Size;
end Graphics.Internals;
