------------------------------------------------------------------------------
--  File:            svgeffio.ads
--  Description:     I/O for Screen.Effects - in fact, for textures
--  Date / Version:  8-Feb-2000
--  Author:          Gautier de Montmollin (gdemont@xoommail.com)
------------------------------------------------------------------------------
with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;

package Screen.Effects.IO is

   procedure Load_BMP_texture(S: Stream_Access; Texture: out p_Texture_map);

end Screen.Effects.IO;
