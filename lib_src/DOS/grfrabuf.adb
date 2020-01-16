with SVGA.Cheat;

package body Graphics.Frame_Buffer is
   ----------------------
   -- Get_Frame_Buffer --
   ----------------------

   function Get_Frame_Buffer return Frame_Buffer_Pointer is
   begin
      return SVGA.Cheat.Get_Data (Buffer.all);
   end Get_Frame_Buffer;

   -----------------------------
   -- Get_Frame_Buffer_Length --
   -----------------------------

   function Get_Frame_Buffer_Length return Positive is
   begin
      return Positive (Buffer.New_Width * Buffer.New_Height);
   end Get_Frame_Buffer_Length;
end Graphics.Frame_Buffer;
