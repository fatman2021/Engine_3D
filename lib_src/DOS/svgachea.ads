with Graphics.Frame_Buffer;

package SVGA.Cheat is
   use Graphics.Frame_Buffer;

   function Get_Data (Buffer : Screen_Buffer) return Frame_Buffer_Pointer;
end SVGA.Cheat;
