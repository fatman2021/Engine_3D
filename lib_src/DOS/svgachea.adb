with Ada.Unchecked_Conversion;

package body SVGA.Cheat is
   function To_Frame_Buffer is new Ada.Unchecked_Conversion (
     Data_Buffer_Access,
     Frame_Buffer_Pointer
   );

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Buffer : Screen_Buffer) return Frame_Buffer_Pointer is
   begin
      return To_Frame_Buffer (Buffer.Data);
   end Get_Data;
end SVGA.Cheat;

