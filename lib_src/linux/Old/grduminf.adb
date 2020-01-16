with Ada.Integer_Text_IO;
with Interfaces.C;

procedure Graphics.Dump_Info ( -- linux
  File : Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output
) is
   use Ada.Integer_Text_IO;
   use Ada.Text_IO;
   use Interfaces.C;

   type Byte is mod 256;

   procedure Clip_Info (context : GraphicsContext_Pointer);

   procedure Clip_Info (context : GraphicsContext_Pointer) is
   begin
      if context.clip /= 0 then
         Put_Line (File, "  Clipping enabled");
         Put (File, "  Top left. X: ");
         Put (File, Integer (context.clipx1), Width => 0);
         Put (File, "; Y: ");
         Put (File, Integer (context.clipy1), Width => 0);
         New_Line (File);
         Put (File, "  Bottom right. X: ");
         Put (File, Integer (context.clipx2), Width => 0);
         Put (File, "; Y: ");
         Put (File, Integer (context.clipy2), Width => 0);
      else
         Put_Line (File, "  Clipping disabled");
      end if;
   end Clip_Info;

   procedure Mode_Info (context : GraphicsContext_Pointer);

   procedure Mode_Info (context : GraphicsContext_Pointer) is
      modeflags : Byte := Byte (context.modeflags);
   begin
      Put (File, "  Mode: ");
      case context.modetype is
         when 0 =>
            Put_Line (File, "CONTEXT_VIRTUAL");
         when 1 =>
            Put_Line (File, "CONTEXT_PAGED");
         when 2 =>
            Put_Line (File, "CONTEXT_LINEAR");
         when 3 =>
            Put_Line (File, "CONTEXT_MODEX");
         when 4 =>
            Put_Line (File, "CONTEXT_PLANAR16");
         when others =>
            Put_Line (File, "Unknown");
      end case;
      if (modeflags and 1) /= 0 then
         Put_Line (File, "  MODEFLAG_PAGEFLIPPING_CAPABLE");
      end if;
      if (modeflags and 2) /= 0 then
         Put_Line (File, "  MODEFLAG_TRIPLEBUFFERING_CAPABLE");
      end if;
      if (modeflags and 4) /= 0 then
         Put_Line (File, "  MODEFLAG_PAGEFLIPPING_ENABLED");
      end if;
      if (modeflags and 8) /= 0 then
         Put_Line (File, "  MODEFLAG_TRIPLEBUFFERING_ENABLED");
      end if;
      if (modeflags and 16) /= 0 then
         Put_Line (File, "  MODEFLAG_FLIPPAGE_BANKALIGNED");
      end if;
      if (modeflags and 32) /= 0 then
         Put_Line (File, "  MODEFLAG_32BPP_SHIFT8 or MODEFLAG_24BPP_REVERSED");
      end if;
   end Mode_Info;

   procedure Size_Info (context : GraphicsContext_Pointer);
   
   procedure Size_Info (context : GraphicsContext_Pointer) is
   begin
      Put (File, "  Width: ");
      Put (File, Integer (context.width), Width => 0);
      Put (File, "; Height: ");
      Put (File, Integer (context.height), Width => 0);
      New_Line (File);
      Put (File, "  Bytes per pixel: ");
      Put (File, Integer (context.bytesperpixel), Width => 0);
      Put (File, "; Bits per pixel: ");
      Put (File, Integer (context.bitsperpixel), Width => 0);
      New_Line (File);
      Put (File, "  Num colors: ");
      Put (File, Integer (context.colors), Width => 0);
      New_Line (File);
      Put (File, "  Scanline length: ");
      Put (File, Integer (context.bytewidth), Width => 0);
      New_Line (File);
   end Size_Info;
begin
   Put_Line (File, "Physical screen:");
   Mode_Info (Physical_Screen);
   Size_Info (Physical_Screen);
   Clip_Info (Physical_Screen);
   Put_Line (File, "Buffer:");
   Mode_Info (Buffer);
   Size_Info (Buffer);
   Clip_Info (Buffer);
end Graphics.Dump_Info;
