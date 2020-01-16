------------------------------------------------------------------------------
--  File:            Multkeys.ads        (possibly extracted from PC_PAQS.ZIP)
--  Description:     Keyboard handler for scanning multiple keys
--                      For a test, see tests\temulkey.adb in same archive
--  Date/version:    4.VIII.1999
--  Author's e-mail: gdemont@hotmail.com
--
--  Copyright (c) Gautier de Montmollin 1999
--  CH-2000 Neuchatel
--  SWITZERLAND
--  Permission granted to use this software, without any warranty, for any
--  purpose, provided this copyright note remains attached and unmodified.
------------------------------------------------------------------------------

-- Handler tested on:
--   MS-DOS  7.1  (Win 9x MS-DOS mode)
--   DR-DOS  7.03 (incl. multitasking mode); better with DPMI ON
--   Windows 4.0  (Win 9x GUI mode)
--   Windows NT   (not by me...)

-- Abbreviations:
--   MS: Microsoft format (MS-DOS & MS Windows)
--   DR: Digital Research format (Novell-Caldera-Lineo DR-DOS)

package Multi_keys is

  -- *** Keyboard map

  subtype key_value is Natural range 0..127;

  -- keyboard(i) <=> the key number i is pressed

  keyboard: array(key_value) of boolean:= (others=> false);

  -- some common values

  key_esc  : constant:= 01;
  key_space: constant:= 57;

  key_left : constant:= 75;
  key_right: constant:= 77;
  key_up   : constant:= 72;
  key_down : constant:= 80;
  key_pgup : constant:= 73;
  key_pgdn : constant:= 81;
  key_home : constant:= 71;
  key_end  : constant:= 79;

  key_gray_plus   : constant:= 78;
  key_gray_minus  : constant:= 74;
  key_gray_star   : constant:= 55;
  key_sys_req     : constant:= 84;
  key_scroll_lock : constant:= 70;

  key_F1   : constant:= 59;
  key_F2   : constant:= 60;
  key_F3   : constant:= 61;
  key_F4   : constant:= 62;
  key_F5   : constant:= 63;
  key_F6   : constant:= 64;
  key_F7   : constant:= 65;
  key_F8   : constant:= 66;
  key_F9   : constant:= 67;
  key_F10  : constant:= 68;
  key_F11  : constant:= 87;
  key_F12  : constant:= 88;

  key_ins  : constant:= 82;
  key_del  : constant:= 83;
  key_ctrl : constant:= 29;
  key_alt  : constant:= 56;
  key_lshft: constant:= 42;
  key_rshft: constant:= 54;
  key_caps : constant:= 58;

  procedure Install;
  procedure Uninstall;

  function Pressed_keys return natural;

  function Is_a_key_pressed return boolean;

  -- *** National keyboards (as in DR-DOS Keyb API)

  type kb_country is
    (US, FR, GR, UK, DK, SV, SU, IT, SP, NO, PO, 
     BE, NL, CF, LA, SF, SG, RU, TQ, TF, HU, BR);

  function Detect_country return kb_country; -- through MS/DR Keyb API

  -- You can override the detected country value.
  -- NB: doesn't change any DOS setting.

  country: kb_country;

  -- Converts scan code to country-dependant character (NUL for wrong keys)
  -- in 850 code page encoding

  function Key_ASCII ( scancode: key_value; upper: boolean ) return character;

  -- Returns a rich image of a key
  -- e.g. "Ctrl", "Alt",... "q", "Q",... "ESC", "CR",... ""

  function Key_image ( scancode: key_value; upper: boolean ) return string;

  -- Returns true only *once* per key pression
  function Strike_1 ( scancode: key_value ) return boolean;

end Multi_keys;
