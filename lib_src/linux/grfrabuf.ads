with Graphics.Colors;

package Graphics.Frame_Buffer is -- linux
   pragma Elaborate_Body;

   type Frame_Buffer_Type is array (Natural) of Graphics.Colors.Color_Index;
   --  Use Get_Frame_Buffer_Length to get real length of the frame buffer

   type Frame_Buffer_Pointer is access all Frame_Buffer_Type;

   function Get_Frame_Buffer return Frame_Buffer_Pointer;

   function Get_Frame_Buffer_Length return Positive;
private
   for Frame_Buffer_Type'Component_Size use Graphics.Colors.Color_Index'Size;
   pragma Convention (C, Frame_Buffer_Pointer);
end Graphics.Frame_Buffer;
