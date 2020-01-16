with Screen.IO;

package body Screen.Effects.IO is

   procedure Load_BMP_texture(S: Stream_Access; Texture: out p_Texture_map) is
     width: Integer; height: Integer;

     function bits(n:natural) return natural is
       nn: natural:= n; b: natural:= 0;
       begin
         while nn/=0 loop b:= b+1; nn:= nn / 2; end loop;
         return b;
       end;

     begin
       Screen.IO.Read_BMP_Header( S, width, height );
       declare
--         B: Screen_Buffer( width, height );
--         P: Color_Palette; -- unused here
         P: Graphics.Colors.Palette_Type (Graphics.Colors.Color_Index); -- unused here
       begin
         texture:= New Texture_map( bits(width-1), bits(height-1) );
         Screen.IO.Load_BMP_Palette( S, P );
         Screen.IO.Load_BMP_Image( S, width, height,
         texture.width, texture.height, texture.Data.all);
--         Put_Buffer( B, Texture.all );
       end;
     end Load_BMP_texture;

end Screen.Effects.IO;
