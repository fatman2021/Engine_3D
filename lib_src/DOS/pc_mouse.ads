------------------------------------------------------------------------------
--  File:            PC_Mouse.ads       (possibly extracted from PC_PAQS.ZIP)
--  Description:     Simple mouse interface for PC  (GNAT/DOS Ada compiler)
--  Date/version:    12.IV.1999 / 28.III.1999 / 29.3.1997
--  Author:          Gautier.deMontmollin@Maths.UniNe.CH
--  Uses:            DJGPP_Library (V0.5 by Jerry van Dijk)
------------------------------------------------------------------------------

package PC_Mouse is

  PROCEDURE MouseReset;                               -- Always begin with it!
  MouseInstalled: Boolean;                            -- (Re)set by MouseReset

  PROCEDURE ShowMouse;
  PROCEDURE HideMouse;
  PROCEDURE MouseOn  renames ShowMouse;
  PROCEDURE MouseOff renames HideMouse;
  PROCEDURE SetSpeed(hr, vr : integer);                   -- Set speed factors
  PROCEDURE SetAccel(threshold: integer);                 -- Set accel. factor
  PROCEDURE SetArea(Left, Up, Right, Down : integer);     -- Set area
  PROCEDURE SetPosition(hPos, vPos : integer);            -- Set position
  PROCEDURE Mouse(hPos, vPos, Status: out integer);       -- Read x,y,status 
  PROCEDURE MouseXY(hPos, vPos: out integer);             -- Read x,y
  FUNCTION  MouseStatus return integer;                   -- Read status

  ButtonL: constant integer := 0;
  ButtonR: constant integer := 1;
  ButtonM: constant integer := 2;

  FUNCTION  MouseClick(Button: integer) return integer;   -- # Clicks
  PROCEDURE MouseLRClick(l,r: out integer);               --   ", left & right

  -- (1999) Clean methods to reflect button status:

  type Mouse_button is (left, right, middle);
  type Mouse_button_bunch is array( Mouse_button ) of boolean;

  function Is_button_pressed(b: Mouse_button) return boolean;
  -- all buttons:
  procedure Which_buttons_pressed( btns: out Mouse_button_bunch );
  -- all at once:
  procedure Mouse(hPos, vPos: out integer; btns: out Mouse_button_bunch);

-- Text mode mouse instructions:

  PROCEDURE SetTextArea(Left, Up, Right, Down: integer);
  PROCEDURE TextMouse(hPos, vPos, Status: out integer);   -- use for text mode
  PROCEDURE TextMouseXY(hPos, vPos: out integer);         -- idem
  
-- Graphic mode mouse instructions:

  type  Std_GCursor is (arrow, inverted_arrow, 
                        check, cross, finger, i_beam,
                        left, right, up, down,
                        horizontal,vertical, diagonal_1,diagonal_2);
                        
  type  G_mouse_bitmap is ARRAY(0..15) OF integer;
  type  GCursor is RECORD
                     ScreenMask, CursorMask : G_mouse_bitmap;
                     hotX, hotY : integer;
                   END record;

  PROCEDURE G_Initialize(x1,y1,x2,y2:integer);
  PROCEDURE G_StdCursor(cursor: Std_GCursor);
  PROCEDURE G_SetCursor(cursor: GCursor);
  PROCEDURE G_ConditionalHide(left, top, right, bottom: integer);

end PC_Mouse;
