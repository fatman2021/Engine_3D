with Graphics.Colors;

package Graphics.Frame_Buffer is -- DOS
   type Frame_Buffer_Type is array (Natural range <>)
     of Graphics.Colors.Color_Index;

   type Frame_Buffer_Pointer is access all Frame_Buffer_Type;

   function Get_Frame_Buffer return Frame_Buffer_Pointer;

   function Get_Frame_Buffer_Length return Positive;
end Graphics.Frame_Buffer;
