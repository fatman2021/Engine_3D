with Interfaces;                        use Interfaces;
with Graphics.Drawing;
with Graphics.Modes;

package body Screen.IO is
   use Graphics;

  ------------------------------------
  -- BMP format I/O                 --
  --                                --
  -- Rev 1.4  11/02/99  RBS         --
  --                                --
  ------------------------------------
  -- Coded by G. de Montmollin

  -- Code additions, changes, and corrections by Bob Sutton
  --
  -- Remarks expanded and altered
  -- Provided for scanline padding in data stream
  -- Corrected stream reading for images exceeding screen size.
  -- Provided selectable trim modes for oversize images
  -- Procedures originally Read_BMP_dimensions now Read_BMP_Header
  -- Some exceptions added
  --
  -- Rev 1.2  RBS.  Added variable XY screen location for BMP
  -- Rev 1.3  RBS.  Added image inversion & reversal capability
  -- Rev 1.4  RBS.  Activated LOCATE centering / clipping options
  --
  -- This version presumes that the infile is a new style, 256 color bitmap.
  -- The Bitmap Information Header structure (40 bytes) is presumed
  -- instead of the pre-Windows 3.0 Bitmap Core Header Structure (12 Bytes)
  -- Pos 15 (0EH), if 28H, is valid BIH structure.  If 0CH, is BCH structure.

  -- Place_image is provided for image mirror and invert options
     Type Place_Img is
        (Img_normal,
         Img_mirror,
         Img_invert,
         Img_mirror_invert);

  -- Locate is provided for alternatives in locating & clipping
  -- oversized BMP images to fit video screen
     Type Locate is
        (Horiz_center_btm_clip,
         Horiz_center_top_clip,
         Vert_center_right_clip,
         Vert_center_left_clip,
         Image_center);

  procedure Read_BMP_Header (S: Stream_Access;
                             width:  out Integer;
                             height: out Integer;
                             os:     out unsigned_32 ) is

     fsz: unsigned_32;
     ih: unsigned_32;
     w: unsigned_16;
     n: unsigned_32;
     Str2:  string(1..2);
     Str4:  string(1..4);
     Str20: string(1..20);

     begin
        --   First 14 bytes is file header structure.
        --   Pos= 1,  read 2 bytes, file signature word
        String'Read(S, Str2);
        if Str2 /= "BM" then
           raise Not_BMP_format;
        end if;
        --   Pos= 3,  read the file size
        unsigned_32'Read(S, fsz);
        --   Pos= 7, read four bytes, unknown
        String'Read(S, Str4);
        --   Pos= 11, read four bytes offset, file top to bitmap data.
        --            For 256 colors, this is usually 36 04 00 00
        unsigned_32'Read(S, os);

        --   Pos= 15. The beginning of Bitmap information header.
        --		    Data expected:  28H, denoting 40 byte header
        unsigned_32'Read(S, ih);
        --   Pos= 19. Bitmap width, in pixels.  Four bytes
        unsigned_32'Read(S,n); width:=  Integer(n);
        --   Pos= 23. Bitmap height, in pixels.  Four bytes
        unsigned_32'Read(S,n); height:= Integer(n);
        --   Pos= 27, skip two bytes.  Data is number of Bitmap planes.
        unsigned_16'Read(S,w); -- perform the skip
        --   Pos= 29, Number of bits per pixel
        --   Value 8, denoting 256 color, is expected
        unsigned_16'Read(S,w);
        if w/=8 then
           raise Unsupported_bits_per_pixel;
        end if;
        --   Pos= 31, read four bytees
        unsigned_32'Read(S,n);                --Type of compression used
        if n /= 0 then
           raise Unsupported_compression;
        end if;

        --   Pos= 35 (23H), skip twenty bytes
        string'read(S, Str20);     -- perform the skip

        --   Pos= 55 (36H), - start of palette
     end Read_BMP_Header;


  -- Read picture header from a BMP stream (exported procedure)
  procedure Read_BMP_Header (S: Stream_Access;
                             width:  out Natural;
                             height: out Natural) is
  os: unsigned_32;
  begin
     Read_BMP_Header(S, width, height, os);
  end Read_BMP_Header;

  -- Read picture header from a BMP file

  procedure Read_BMP_Header(Name:      String;
                            width:  out Natural;
                            height: out Natural) is
  os : unsigned_32;
  f: File_Type;

     begin
        Open(f, in_file, Name);
        Read_BMP_Header(Stream(f), width, height, os);
        Close(f);
     end Read_BMP_Header;

  -- Load palette only, from BMP file
  -- This reads RGB Quad Structure (BGR order)

--  procedure Load_BMP_Palette (S       : Stream_Access;
--                              Palette : out Color_Palette) is
  procedure Load_BMP_Palette (S       : Stream_Access;
                              Palette : out Graphics.Colors.Palette_Type) is
     use Graphics.Colors;
     d: byte;
     rgb : Colors.RGB_Color_Type;

     function To_Intensity (B : Byte) return Colors.Intensity_Type is
        Spare : Float := Float (B) / 256.0;
     begin
        return Colors.Intensity_Type (Spare);
     end To_Intensity;

     begin
--        for DAC in Color_Type loop
        for DAC in Color_Index loop
--           Byte'Read(S,d); Palette(DAC).Blue  := Color_Value(d / 4);
--           Byte'Read(S,d); Palette(DAC).Green := Color_Value(d / 4);
--           Byte'Read(S,d); Palette(DAC).Red   := Color_Value(d / 4);
           Byte'Read(S,d); rgb (Blue)  := To_Intensity (d);
           Byte'Read(S,d); rgb (Green) := To_Intensity (d);
           Byte'Read(S,d); rgb (Red)   := To_Intensity (d);
           Palette (DAC) := To_Color (rgb);
           Byte'Read(S,d);
         end loop;
     end Load_BMP_Palette;

--  procedure Load_BMP_palette (Name    : String;
--                              Palette : out Color_Palette) is
  procedure Load_BMP_palette (Name    : String;
                              Palette : out Graphics.Colors.Palette_Type) is
     f: File_Type;
     S: Stream_Access;
     width: Integer; height: Integer;
	 os: unsigned_32;

     begin
        Open(f, in_file, Name);
        S:= Stream(f);
        Read_BMP_Header(S, width, height, os);
        Load_BMP_Palette(S, Palette);
        Close(f);
     end Load_BMP_palette;

  -- Load image only from stream (after having read header and palette!)

--  procedure Load_BMP_Image (S       : Stream_Access;
--                            width   : in Integer;
--                            height  : in Integer;
--                            Buffer  : in out Screen_Buffer) is
  procedure Load_BMP_Image (S        : Stream_Access;
                            width    : in Natural;
                            height   : in Natural;
                            b_width  : in Natural;
                            b_height : in Natural;
                            buffer   : out Data_Buffer) is
     w: Integer:= width;
     h: Integer:= height;
     Idx: Natural;

     begin
        if b_width * b_height > buffer'Length then
           raise Graphics.Graphics_Error;
        end if;
--        if w > buffer.width  then w:= buffer.width;  end if;
--        if h > buffer.height then h:= buffer.height; end if;
        if w > b_width  then w:= b_width;  end if;
        if h > b_height then h:= b_height; end if;
        for y in reverse 0..h-1 loop
--           Idx:= y * buffer.width;
           Idx:= y * b_width;
--           Data_Buffer'Read(S, Buffer.Data(Idx..Idx+w-1));
           declare
              b : Byte;
           begin
              -- Data_Buffer'Read seems to be reading array bounds first!
              for J in Idx .. Idx + w - 1 loop
--           Data_Buffer'Read(S, buffer(Idx..Idx+w-1));
                 Byte'Read(S, b);
                 buffer (J) := Graphics.Colors.Color_Index (b);
              end loop;
           end;
        end loop;
     end Load_BMP_Image;

  -- Load the contents of full BMP from a stream to buffer & palette

--  procedure Load_BMP (S       : Stream_Access;
--                      Buffer  : in out Screen_Buffer;
--                      Palette : out Color_Palette ) is
  procedure Load_BMP (S       : Stream_Access;
                      Palette : out Graphics.Colors.Palette_Type) is

     width: Integer; height: Integer;
     offset : unsigned_32;
     begin
        Read_BMP_Header (S, width, height, offset);
        Load_BMP_Palette(S, Palette);
--QQ        Load_BMP_Image  (S, width, height, Buffer);
     end Load_BMP;

  -- Load the contents of full BMP file to buffer & palette

--  procedure Load_BMP (Name:     String;
--                      Buffer:   in out Screen_Buffer;
--                      Palette:  out Color_Palette ) is
  procedure Load_BMP (Name:     String;
                      Palette:  out Graphics.Colors.Palette_Type) is

     f: File_Type;

     begin
        Open(f, in_file, Name);
--        Load_BMP( Stream(f), Buffer, Palette );
        Load_BMP( Stream(f), Palette );
        Close (f);
     exception
        when others => Close(f); raise;
     end Load_BMP;

  -- Load the contents of BMP file stream to screen
  procedure Load_BMP (S: Stream_Access;
                      Image_X_loc   : IN Integer := 0;
                      Image_Y_loc   : IN Integer := 0;
                      Image_place   : IN Place_Img := Img_normal;
                      Trim_BMP      : IN Locate := Image_center ) is



--     Palette       : Color_Palette;
     Palette       : Graphics.Colors.Palette_Type (Graphics.Colors.Color_Index);
     b             : Byte;
     c             : Graphics.Colors.Color_Index;
     offset        : unsigned_32;
     width         : Integer;
     height        : Integer;
     x_idx         : Integer;
     y_idx         : Integer;
     clip_left     : Integer;
     clip_right    : Integer;
     clip_top      : Integer;
     clip_btm      : Integer;
     trim_h        : integer;
     trim_w        : integer;
     trim_x_cntr   : Boolean := True;
     trim_left     : Boolean := False;
     trim_right    : Boolean := False;
     trim_y_cntr   : Boolean := True;
     trim_top      : Boolean := False;
     trim_btm      : Boolean := False;

     begin
        case trim_BMP is
           when Horiz_center_btm_clip =>
               trim_x_cntr  := True;
               trim_left    := False;
               trim_right   := False;
               trim_y_cntr  := False;
               trim_top     := False;
               trim_btm     := True;

           when Horiz_center_top_clip =>
               trim_x_cntr  := True;
               trim_left    := False;
               trim_right   := False;
               trim_y_cntr  := False;
               trim_top     := True;
               trim_btm     := False;

           when Vert_center_right_clip =>
               trim_x_cntr  := False;
               trim_left    := False;
               trim_right   := True;
               trim_y_cntr  := True;
               trim_top     := False;
               trim_btm     := False;

           when Vert_center_left_clip =>
               trim_x_cntr  := False;
               trim_left    := True;
               trim_right   := False;
               trim_y_cntr  := True;
               trim_top     := False;
               trim_btm     := False;

           when others =>		           -- Includes Image_center
               trim_x_cntr  := True;
               trim_left    := False;
               trim_right   := False;
               trim_y_cntr  := True;
               trim_top     := False;
               trim_btm     := False;
        end case;

        Read_BMP_Header(S, width, height, offset);  -- Positions to Palette
        If offset /= 16#0436# then
           raise Unsupported_BMP_format;
        end if;
--		If Image_X_loc >= X_Size then
		If Image_X_loc >= Graphics.Modes.Get_Width then
		   raise Image_X_offset_too_great;
		end if;

--		If Image_Y_loc >= Y_Size then
		If Image_Y_loc >= Graphics.Modes.Get_Height then
		   raise Image_Y_offset_too_great;
		end if;

        Load_BMP_Palette(S, Palette);
--        Set_Palette( Palette );
        Graphics.Colors.Set_Palette( Palette );

        -- Get length of any padding to bypass at line right end
        clip_right := 4 - width MOD 4;
        if clip_right = 4 then clip_right := 0; End If;

        -- Limit BMP line length to screen mode width, & bypass padding
        clip_left := 0;
        trim_w := 0;
--        if width + Image_X_loc  > X_Size then
        if width + Image_X_loc  > Graphics.Modes.Get_Width then
--           trim_w := width + Image_X_loc - X_size;
           trim_w := width + Image_X_loc - Graphics.Modes.Get_Width;
--           width := X_Size;
           width := Graphics.Modes.Get_Width;
           if trim_left then clip_left := trim_w; end if;
           if trim_right then
              clip_right := clip_right + trim_w;
           end if;
           if trim_x_cntr then
              clip_left := trim_w / 2;      -- favor right trim
              clip_right := clip_right + trim_w - clip_left;
           end if;
        end if;

        -- Total number of bytes to remove from line data
        trim_w := clip_left + clip_right;

        -- Limit BMP number of lines to screen mode heigth
        clip_top := 0; clip_btm := 0;
        trim_h := 0;
--        if height + Image_Y_loc > Y_Size then
        if height + Image_Y_loc > Graphics.Modes.Get_Height then
--           trim_h := height + Image_Y_loc - Y_size;
           trim_h := height + Image_Y_loc - Graphics.Modes.Get_Height;
--           height := Y_Size;
           height := Graphics.Modes.Get_Height;
           if trim_btm then clip_btm := trim_h; end if;
           if trim_top then clip_top := trim_h; end if;
           if trim_y_cntr then
              clip_top := trim_h / 2;
              clip_btm := trim_h - clip_top;   -- favor bottom trim
           end if;
        end if;

        -- Bitmap loads from bottom left.  Y origin is at bottom
        -- Bypass bottom lines, if image is too high
        for y in 1..clip_btm loop           -- BOTTOM CLIP LINES
           for x in 1..width + trim_w loop  -- number of line bytes to skip
              Byte'Read(S, b);        -- advance the read point
           end loop;
      	end loop;

        -- Bottom trim of image is complete.
        -- Stream read point is at origin of display image, bottom line.

        -- Process the image lines.  Truncate and depad as required
        -- Y must decrement, or image will be inverted
        for y in reverse Image_Y_loc..height -1 loop  -- do the image scan lines
           if clip_left > 0 then               -- LEFT CLIP PIXELS
              for x in 1..clip_left loop       -- length of data to skip
                 Byte'Read(S, b);        -- advance the read point
              end loop;
           end if;

           -- Display the image
           -- X must increment or image will be reversed
           for x in Image_X_loc..width -1 loop    -- display the line
              case Image_place is
                  when Img_normal =>
                     x_idx := x;
                     y_idx := y;
                  when Img_mirror =>
                     x_idx := width -1 -x;
                     y_idx := y;
                  when Img_invert =>
                     x_idx := x;
                     y_idx := height -1 - y;
                  when Img_mirror_invert =>
                     x_idx := width -1 - x;
                     y_idx := height -1 - y;
                  when others =>
                     x_idx := x;
                     y_idx := y;
              end case;
--              Color_Type'Read(S, c); Set_Pixel(x_idx, y_idx, c);
              Byte'Read(S, b);
              c := Graphics.Colors.Color_Index (b);
              Graphics.Drawing.Set_Pixel(x_idx, y_idx, c);
           end loop;

           -- Perform bypass of any line padding at normal right end
           if clip_right > 0 then                 -- RIGHT CLIP PIXELS
              for x in 1..clip_right loop         -- length of data to skip
                 Byte'Read(S, b);           -- advance the read point
              end loop;
           end if;
        end loop;

        -- Bypass top lines, if image is too high
        if clip_top > 0 then                      -- trim at top, if too high
      	   for y in 1..clip_top loop              -- TOP CLIP LINES
              for x in 1..width + trim_w loop     -- length of line data to skip
                 Byte'Read(S, b);           -- advance the read point
              end loop;
      	   end loop;
        end if;
    end Load_BMP;

 --  Load the image file, filename.BMP as a stream
  procedure Load_BMP (Name: String) is
     f: File_Type;
     begin
        Open(f, in_file, Name);
        Load_BMP( Stream(f) );
        Close(f);
     exception
        when others => Close(f); raise;
     end Load_BMP;

  -- BMP Output

  generic
--    with function G_Get_Pixel(X: Integer; Y: Integer) return Color_Type;
    with function G_Get_Pixel(X: Integer; Y: Integer) return Graphics.Colors.Color_Index;
    max_width  : Integer;
    max_height : Integer;
--  procedure G_Save_BMP (S       : Stream_Access;
--                        Palette : Color_Palette);
  procedure G_Save_BMP (S       : Stream_Access;
                        Palette : Graphics.Colors.Palette_Type);

--  procedure G_Save_BMP (S       : Stream_Access;
--                        Palette : Color_Palette) is
  procedure G_Save_BMP (S       : Stream_Access;
                        Palette : Graphics.Colors.Palette_Type) is
    use Graphics.Colors;
    line_pad:  integer; -- RBS
    rgb : RGB_Color_Type;
    begin
      line_pad:= (- max_width) MOD 4;
      String'Write(S, "BM");
      unsigned_32'Write(S, 54+1024+
        unsigned_32((max_width + line_pad) * max_height)); -- file size
      unsigned_32'Write(S, 0);
      unsigned_32'Write(S, 1078); -- dist. to data
      -- BMP header
      unsigned_32'Write(S, 40); -- header's size
      unsigned_32'Write(S, unsigned_32(max_width));
      unsigned_32'Write(S, unsigned_32(max_height));
      unsigned_16'Write(S, 1); -- #planes
      unsigned_16'Write(S, 8); -- #bits per pixel
      unsigned_32'Write(S, 0); -- compression
      unsigned_32'Write(S, 0); -- #bits
      unsigned_32'Write(S, 0); -- resx
      unsigned_32'Write(S, 0); -- resy
      unsigned_32'Write(S, 256); -- #colours
      unsigned_32'Write(S, 0);   -- #signifiant colours

      -- 55
--      for DAC in Color_Type loop
      for DAC in Color_Index loop
--        Byte'Write(S, Byte(Palette(DAC).Blue)  * 4);
--        Byte'Write(S, Byte(Palette(DAC).Green) * 4);
--        Byte'Write(S, Byte(Palette(DAC).Red)   * 4);
        rgb := To_RGB_Color (Palette(DAC));
        Byte'Write(S, Byte(rgb(Blue)  * 64)); --QQ get 64 if int = 1.0!
        Byte'Write(S, Byte(rgb(Green) * 64)); --QQ get 64 if int = 1.0!
        Byte'Write(S, Byte(rgb(Red)   * 64)); --QQ get 64 if int = 1.0!
        Byte'Write(S,0);
      end loop;
	  -- save from end of buffer, backwards
      for y in reverse 0..max_height-1 loop
        for x in 0..max_width-1 loop
--          Byte'Write(S, G_Get_Pixel(x,y));
          Byte'Write(S, Byte (G_Get_Pixel(x,y)));
        end loop;
        for x in 1..line_pad loop
          Byte'Write(S, 0);
        end loop;
      end loop;
    end G_Save_BMP;

  -- Save a buffer & palette as a BMP file

--  procedure Save_BMP (S       : Stream_Access;
--                      Buffer  : Screen_Buffer;
--                      Palette : Color_Palette) is
  procedure Save_BMP (S       : Stream_Access;
                      Palette : Graphics.Colors.Palette_Type) is

--      function B_Get_Pixel(X: Integer; Y: Integer) return Color_Type is
--        begin
--          return Get_Pixel(Buffer, X,Y);
--        end;
--      procedure B_Save_BMP is
--        new G_Save_BMP( B_Get_Pixel, buffer.width, buffer.height );
      procedure B_Save_BMP is
--        new G_Save_BMP( B_Get_Pixel, Graphics.Modes.Get_Width, Graphics.Modes.Get_Height);
        new G_Save_BMP(Graphics.Drawing.Get_Pixel, Graphics.Modes.Get_Width, Graphics.Modes.Get_Height);

    begin
      B_Save_BMP( S, Palette );
    end Save_BMP;

--  procedure Save_BMP (Name    : String;
--                      Buffer  : Screen_Buffer;
--                      Palette : Color_Palette) is
  procedure Save_BMP (Name    : String;
                      Palette : Graphics.Colors.Palette_Type) is
    f: File_Type;
    begin
      Create(f, out_file, Name);
--      Save_BMP( Stream(f), Buffer, Palette );
      Save_BMP( Stream(f), Palette );
      Close(f);
    end Save_BMP;

  -- Save a screen as a BMP file

  procedure Save_BMP (S: Stream_Access) is
    Spare_pal : Graphics.Colors.Palette_Type (Graphics.Colors.Color_Index);
--    procedure S_Save_BMP is new G_Save_BMP( Get_Pixel, X_Size, Y_Size );
    procedure S_Save_BMP is new G_Save_BMP(Graphics.Drawing.Get_Pixel, Graphics.Modes.Get_Width, Graphics.Modes.Get_Height);
    begin
--      S_Save_BMP( S, Get_Palette );
      Graphics.Colors.Get_Palette (Spare_pal);
      S_Save_BMP( S, Spare_pal);
    end Save_BMP;

  procedure Save_BMP (Name: String) is
    f: File_Type;
    begin
      Create(f, out_file, Name);
      Save_BMP( Stream(f) );
      Close(f);
    end Save_BMP;

  ----------------------
  -- GIF format Input --
  ----------------------
  -- Pascal code by Steven H Don, shd@earthling.net, http://shd.cjb.net .
  -- Translated via p2ada. Added: interlaced images / skip extended blocks

--  SUBTYPE GIF_signature IS String(1..6);
--
--  TYPE GIFHeader  IS   RECORD
--    Signature                            : GIF_signature;
--    ScreenWidth, ScreenHeight            : unsigned_16;   --| Logical screen |
--    Depth, Background, Aspect_ratio_code : unsigned_8;    --|   descriptor   |
--  END RECORD;
--
--  TYPE GIFDescriptor  IS   RECORD
--    ImageLeft,
--    ImageTop,
--    ImageWidth,
--    ImageHeight : unsigned_16;
--    Depth : unsigned_8;
--  END RECORD;
--
--
--  procedure Read_GIF_palette         -- rather internal
--              (S: Stream_Access; Last_colour: Color_type;
--               Palette: out Color_Palette ) is
--    d: Byte;
--    begin
--      for DAC in 0..Last_colour loop
--        Byte'Read(S,d); Palette(DAC).Red   := Color_Value(d / 4);
--        Byte'Read(S,d); Palette(DAC).Green := Color_Value(d / 4);
--        Byte'Read(S,d); Palette(DAC).Blue  := Color_Value(d / 4);
--      end loop;
--    end Read_GIF_palette;
--
--  procedure Read_GIF_dimensions_and_palette
--    (S       : Stream_Access;
--     Num_of_colours : out Natural;
--     width   : out Integer;
--     height  : out Integer;
--     Palette : out Color_Palette ) is
--
--    Header: GIFHeader;
--    -- Colour information
--    BitsPerPixel: Natural;
--    Last_colour : Color_Type;
--
--    begin
--      GIFHeader'Read( S, Header );
--
--      -- Check signature and terminator
--      IF Header.Signature /= "GIF87a" AND Header.Signature /= "GIF89a" THEN
--        raise not_GIF_format;
--      END IF;
--
--      width := Integer(Header.ScreenWidth);
--      height:= Integer(Header.ScreenHeight);
--
--      -- Get amount of colours in image
--      BitsPerPixel := 1 + Natural(Header.Depth  AND  7);
--      Last_colour := 2 ** BitsPerPixel - 1;
--      Num_of_colours:= 2 ** BitsPerPixel;
--
--      if (Header.Depth and 128) > 0 then -- Global colour map
--        Read_GIF_palette( S, Last_colour, Palette );
--      end if;
--
--    end Read_GIF_dimensions_and_palette;
--
--  -- Read picture dimensions from a GIF file
--
--  procedure Read_GIF_dimensions_and_palette
--    (Name:      String;
--     Num_of_colours : out Natural;
--     width:  out Integer;
--     height: out Integer;
--     Palette : out Color_Palette ) is
--    f: File_Type;
--    begin
--      Open(f, in_file, Name);
--      Read_GIF_dimensions_and_palette(Stream(f),
--        Num_of_colours, width, height, Palette);
--      Close(f);
--    end Read_GIF_dimensions_and_palette;
--
--  -- Load the raw image of GIF file. Generic procedure for screen / buffer
--
--  generic
--    with procedure G_Set_Pixel(X: Integer; Y: Integer; Color: Color_Type);
--    max_width  : Integer;
--    max_height : Integer;
--
--  procedure G_Load_GIF_raw_image
--      (S       : Stream_Access;
--       Num_of_colours : Natural;
--       width   : in out Integer;
--       height  : in out Integer );
--
--  procedure G_Load_GIF_raw_image
--      (S              : Stream_Access;
--       Num_of_colours : Natural;
--       width          : in out Integer;
--       height         : in out Integer ) is
--
--    type T_Stream_Buffer is ARRAY ( Integer range <> ) OF Byte;
--    Stream_Buffer : T_Stream_Buffer( 0..256 );
--
--    -- For loading from the GIF file
--    Descriptor     : GIFDescriptor;
--    Temp           : Byte;
--    BPointer_min_1 : Integer;
--
--    -- Coordinates
--    X, tlX, brX : Integer;
--    Y, tlY, brY : Integer;
--
--    -- GIF data is stored in blocks of a certain size
--    BlockSize      : Natural;
--    BlockSize_min_1: Integer;
--
--    -- The string table
--    Prefix       : ARRAY ( 0..4096 ) OF Natural;
--    Suffix       : ARRAY ( 0..4096 ) OF Natural;
--    OutCode      : ARRAY ( 0..1024 ) OF Natural;
--    FirstFree,
--    FreeCode     : Natural;
--
--    -- All the code information
--    InitCodeSize,
--    CodeSize     : Natural;
--    Code,
--    OldCode,
--    MaxCode      : Natural;
--
--    -- Special codes
--    ClearCode,
--    EOICode      : Natural;
--
--    -- Used while reading the codes
--    BitsIn       : Byte;
--
--    -- Interlaced images
--
--    Interlaced     : boolean;
--    Interlace_pass : Natural range 1..4:= 1;
--    Span           : Natural:= 7;
--    -- display interlaced like when surfing on the Web (a bit slower) :
--    Nice_drawing   : constant boolean:= true;
--
--    Separator :  Character ; -- block separator - "," for image, "!" for exten.
--
--    -- Colour information
--    New_num_of_colours : Natural;
--    Pixel_mask : Color_type;
--    -- This is in case of local colour map)
--    BitsPerPixel  : Natural;
--    Trash_Palette : Color_Palette;
--
--    -- Local function to read from the buffer
--    FUNCTION  LoadByte  RETURN  Byte IS
--      BEGIN
--        -- Read next block
--        IF  BPointer_min_1 = BlockSize_min_1 THEN
--          begin
--            T_Stream_Buffer'Read(S, Stream_Buffer(0..BlockSize));
--          exception
--            when End_error => null; -- nothing bad, just EOF hit
--          end;
--          BPointer_min_1 := -1;
--        END IF;
--
--        -- Return byte
--        BPointer_min_1:= BPointer_min_1 + 1;
--        RETURN Stream_Buffer (BPointer_min_1);
--      END LoadByte;
--
--    -- Local procedure to read the next code from the file
--    PROCEDURE ReadCode IS
--      Bit_Mask: Natural:= 1;
--      BEGIN
--        Code := 0;
--        -- Read the code, bit by bit
--        FOR  Counter  IN REVERSE  0..CodeSize - 1  LOOP
--          -- Next bit
--          BitsIn:= BitsIn + 1;
--
--          -- Maybe, a new byte needs to be loaded with a further 8 bits
--          IF  BitsIn = 9 THEN
--            Temp := LoadByte;
--            BitsIn := 1;
--          END IF;
--
--          -- Add the current bit to the code
--          IF  (Temp  AND  1) > 0 THEN
--            Code:= Code + Bit_Mask;
--          END IF;
--          Bit_Mask := Bit_Mask * 2;
--          Temp     := Temp / 2;
--        END LOOP;
--      END ReadCode;
--
--    -- Local procedure to draw a pixel
--    PROCEDURE NextPixel( code: Natural) IS
--      c : Color_Type:= Color_Type(code) and Pixel_mask;
--      BEGIN
--        -- Actually draw the pixel on screen buffer
--        if X < width then
--          if Nice_drawing and then Interlaced then
--            for i in reverse 0..Span loop
--              if Y+i < height then
--                G_Set_Pixel(X,Y+i, c);
--              end if;
--            end loop;
--          else
--            if Y < height then
--              G_Set_Pixel(X,Y, c);
--            end if;
--          end if;
--        end if;
--
--        -- Move on to next pixel
--        X:= X + 1;
--
--        -- Or next row, if necessary
--        IF  X = brX THEN
--          X:= tlX;
--          if Interlaced then
--            case Interlace_pass is
--              when 1 =>
--                 Y:= Y + 8;
--                 if Y >= brY then Y:= 4; Interlace_pass:= 2; Span:= 3; end if;
--              when 2 =>
--                 Y:= Y + 8;
--                 if Y >= brY then Y:= 2; Interlace_pass:= 3; Span:= 1; end if;
--              when 3 =>
--                 Y:= Y + 4;
--                 if Y >= brY then Y:= 1; Interlace_pass:= 4; Span:= 0; end if;
--              when 4 =>
--                 Y:= Y + 2;
--            end case;
--          else
--            Y:= Y + 1;
--          end if;
--        END IF;
--      END NextPixel;
--
--      -- Local function to output a string. Returns the first character.
--      FUNCTION  OutString (CurCode_1: Natural) RETURN Natural IS
--        CurCode  : Natural:= CurCode_1;
--        OutCount : Natural;
--
--        BEGIN
--          -- If it's a single character, output that
--          IF  CurCode < New_num_of_colours THEN
--            NextPixel( CurCode );
--          ELSE
--            OutCount := 0;
--
--            -- Store the string, which ends up in reverse order
--            LOOP
--              OutCode (OutCount) := Suffix (CurCode);
--              OutCount:= OutCount + 1;
--              CurCode := Prefix (CurCode);
--              EXIT WHEN  CurCode < New_num_of_colours;
--            END LOOP;
--
--            -- Add the last character
--            OutCode (OutCount) := CurCode;
--            OutCount:= OutCount + 1;
--
--            -- Output all the string, in the correct order
--            LOOP
--              OutCount:= OutCount - 1;
--              NextPixel( OutCode(OutCount) );
--              EXIT WHEN  OutCount = 0;
--            END LOOP;
--
--          END IF;
--          -- Return 1st character
--          return CurCode;
--      END OutString;
--
--    begin
--      New_num_of_colours:= Num_of_colours;
--      Pixel_mask:= Color_Type(New_num_of_colours - 1);
--
--      if width  > max_width  then width:=  max_width;  end if;
--      if height > max_height then height:= max_height; end if;
--
--      -- Skip all extensions until finding an image descriptor
--
--      loop
--        character'Read( S, Separator );
--        case Separator is
--          when ',' => exit; -- image descriptor will begin !
--          when '!' =>
--            Byte'Read( S, temp ); -- skip extension identifier byte
--            loop
--              Byte'Read( S, temp ); -- load sub-block length byte
--              exit when temp = 0;   -- null sub-block -> the end!
--              for i in reverse 1..temp loop
--                Byte'Read( S, temp ); -- load sub-block byte
--              end loop;
--            end loop;
--          when others => raise unknown_GIF_separator;
--        end case;
--      end loop;
--
--      -- We load now 1st image only!
--
--      -- Load the image descriptor
--      GIFDescriptor'Read( S, Descriptor );
--
--      -- Get image corner coordinates
--      tlX := Integer(Descriptor.ImageLeft);
--      tlY := Integer(Descriptor.ImageTop);
--      brX := tlX + Integer(Descriptor.ImageWidth);
--      brY := tlY + Integer(Descriptor.ImageHeight);
--
--      Interlaced:= (Descriptor.Depth  AND  64) = 64;
--
--      -- Local colour table
--      IF  (Descriptor.Depth  AND  128) = 128 THEN
--        -- Get amount of colours in image
--        BitsPerPixel := 1 + Natural(Descriptor.Depth  AND  7);
--        New_num_of_colours:= 2 ** BitsPerPixel;
--        Pixel_mask:= Color_Type(New_num_of_colours - 1);
--        Read_GIF_palette( S, Pixel_mask, Trash_Palette );
--      END IF;
--
--      -- Get initial code size
--      Byte'Read( S, temp ); CodeSize := Natural(temp);
--
--      -- GIF data is stored in blocks, so it's necessary to know the size
--      Byte'Read( S, temp ); BlockSize:= Natural(temp);
--      BlockSize_min_1:= BlockSize - 1;
--
--      -- Start loader
--      BPointer_min_1 := BlockSize_min_1;
--
--      -- Special codes used in the GIF spec
--      ClearCode        := 2 ** CodeSize;     -- Code to reset
--      EOICode          := ClearCode + 1;     -- End of file
--
--      -- Initialize the string table
--      FirstFree        := ClearCode + 2;     -- Strings start here
--      FreeCode         := FirstFree;         -- Strings can be added here
--
--      -- Initial size of the code and its maximum value
--      CodeSize      := CodeSize + 1;
--      InitCodeSize  := CodeSize;
--      MaxCode       := 2 ** CodeSize;
--
--      BitsIn := 8;
--
--      -- Start at top left of image
--      X := Integer(Descriptor.ImageLeft);
--      Y := Integer(Descriptor.ImageTop);
--
--      LOOP
--        -- Read next code
--        ReadCode;
--
--        -- If it's an End-Of-Information code, stop processing
--        exit when Code = EOICode;
--
--        -- If it's a clear code...
--        IF  Code = ClearCode THEN
--          -- Clear the string table
--          FreeCode := FirstFree;
--
--          -- Set the code size to initial values
--          CodeSize := InitCodeSize;
--          MaxCode  := 2 ** CodeSize;
--
--          -- The next code may be read
--          ReadCode;
--          OldCode := Code;
--
--          -- Set pixel
--          NextPixel( Code );
--
--        -- Other codes
--        ELSE
--
--          -- If the code is already in the string table, it's string is displayed,
--          --   and the old string followed by the new string's first character is
--          --   added to the string table.
--          IF  Code < FreeCode THEN
--            Suffix (FreeCode) := OutString (Code);
--          ELSE
--          -- If it is not already in the string table, the old string followed by
--          --  the old string's first character is added to the string table and
--          --  displayed.
--            Suffix (FreeCode) := OutString (OldCode);
--            NextPixel( Suffix (FreeCode) );
--          END IF;
--
--          -- Finish adding to string table
--          Prefix (FreeCode) := OldCode;
--          FreeCode:= FreeCode + 1;
--
--          -- If the code size needs to be adjusted, do so
--          IF  (FreeCode >= MaxCode)  AND THEN (CodeSize < 12) THEN
--            CodeSize:= CodeSize + 1;
--            MaxCode := MaxCode  * 2;
--          END IF;
--
--          -- The current code is now old
--          OldCode := Code;
--        END IF;
--
--        EXIT WHEN  Code = EOICode;
--      END LOOP;
--    end G_Load_GIF_raw_image;
--
--  procedure Load_GIF_raw_image
--      (S       : Stream_Access;
--       Buffer  : in out Screen_Buffer;
--       Num_of_colours : Natural;
--       width   : in out Integer;
--       height  : in out Integer ) is
--
--      procedure B_Set_Pixel(X: Integer; Y: Integer; Color: Color_Type) is
--        begin
--          Set_Pixel(Buffer, X,Y, Color);
--        end;
--      procedure B_Load_GIF_raw_image is
--        new G_Load_GIF_raw_image( B_Set_Pixel, Buffer.width, Buffer.height);
--
--    begin
--      B_Load_GIF_raw_image( S, Num_of_colours, width, height );
--    end Load_GIF_raw_image;
--
--  procedure Load_GIF_raw_image
--      (S       : Stream_Access;
--       Num_of_colours : Natural;
--       width   : in out Integer;
--       height  : in out Integer ) is
--
--      procedure S_Load_GIF_raw_image is
--        new G_Load_GIF_raw_image( Set_Pixel, X_Size, Y_Size);
--
--    begin
--      S_Load_GIF_raw_image( S, Num_of_colours, width, height );
--    end Load_GIF_raw_image;
--
--  -- Load the contents of GIF file to buffer & palette
--
--  procedure Load_GIF (S       : Stream_Access;
--                      Buffer  : in out Screen_Buffer;
--                      Palette : out Color_Palette) is
--    Num_of_colours : Natural;
--    width: Integer; height: Integer;
--    begin
--      Read_GIF_dimensions_and_palette(S, Num_of_colours,
--        width, height, Palette);
--      Load_GIF_raw_image(S, Buffer, Num_of_colours, width, height);
--    end Load_GIF;
--
--  procedure Load_GIF (Name:     String;
--                      Buffer:   in out Screen_Buffer;
--                      Palette:  out Color_Palette) is
--    f: File_Type;
--    begin
--      Open(f, in_file, Name);
--      Load_GIF( Stream(f), Buffer, Palette );
--      Close (f);
--    exception
--      when others => Close(f); raise;
--    end Load_GIF;
--
--  -- Load the contents of GIF file to screen
--
--  procedure Load_GIF (S: Stream_Access) is
--    Palette : Color_Palette;
--    Num_of_colours : Natural;
--    width: Integer; height: Integer;
--    begin
--      Read_GIF_dimensions_and_palette(S, Num_of_colours,
--        width, height, Palette);
--      Set_Palette( Palette );
--      Load_GIF_raw_image(S, Num_of_colours, width, height);
--    end Load_GIF;
--
--  procedure Load_GIF (Name: String) is
--    f: File_Type;
--    begin
--      Open(f, in_file, Name);
--      Load_GIF( Stream(f) );
--      Close(f);
--    exception
--      when others => Close(f); raise;
--    end Load_GIF;
--
end Screen.IO;
