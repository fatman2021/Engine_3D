with Ada.Unchecked_Conversion;

package body Graphics.Frame_Buffer is -- linux
   ----------------------
   -- Get_Frame_Buffer --
   ----------------------

   function Get_Frame_Buffer return Frame_Buffer_Pointer is
      function To_Frame_Buffer is new Ada.Unchecked_Conversion (
        Source => chars_ptr,
        Target => Frame_Buffer_Pointer
      );
   begin
      return To_Frame_Buffer (Buffer.vbuf);
   end Get_Frame_Buffer;

   -----------------------------
   -- Get_Frame_Buffer_Length --
   -----------------------------

   function Get_Frame_Buffer_Length return Positive is
   begin
      return Positive (Buffer.width * Buffer.height);
   end Get_Frame_Buffer_Length;
end Graphics.Frame_Buffer;
