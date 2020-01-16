-----------------------------------------------------------------------------
--  File: multkeys.adb; see specification (multkeys.ads)
-----------------------------------------------------------------------------
with DOS_Interrupts;
with DJGPP_Library;                     use DJGPP_Library;
with System.Machine_Code;               use System.Machine_Code;
with Interfaces;                        use Interfaces;

package body Multi_keys is
   use ASCII;

   function Inport (Port : Unsigned_16) return Unsigned_8 is
      Value : Unsigned_8;
   begin
      ASM ("inb %%dx",
        Unsigned_8'Asm_Output ("=a", Value),
        Unsigned_16'Asm_Input ("d", Port),
        Volatile => True);
      return Value;
   end Inport;
   pragma Inline (Inport);

   procedure Outport (Port : in Unsigned_16; Value : in Unsigned_8) is
   begin
      ASM ("outb %%dx",
        No_Output_Operands,
        (Unsigned_8'Asm_Input  ("a", Value),
         Unsigned_16'Asm_Input ("d", Port)));
   end Outport;
   pragma Inline (Outport);

   -----------------------------------------------------------

 procedure New_int_09_handler is
   Key, Dummy: unsigned_8;
   begin
     Key:=   Inport(16#60#);
     Dummy:= Inport(16#61#);
     Outport(16#61#, Dummy or 16#80#);
     Outport(16#61#, Dummy);
     keyboard( integer(key and 16#7F#) ):= (key and 16#80#) = 0;
     Outport(16#20#, 16#20#);
  end New_int_09_handler;

  installed: boolean:= false;

  procedure Install is
    begin
      if not installed then
        Dos_Interrupts.Set_Interrupt( 09, New_int_09_handler'address );
        installed:= true;
      end if;
    end;

  procedure Uninstall is
    begin
      if installed then
        Dos_Interrupts.Remove_Interrupt( 09 );
        installed:= false;
      end if;
    end;

  function Pressed_keys return natural is
    cnt: natural:= 0;
    begin
      for i in keyboard'range loop
        if keyboard(i) then cnt:= cnt+1; end if;
      end loop;
      return cnt;
    end;

  function Is_a_key_pressed return boolean is
    begin
      return Pressed_keys > 0;
    end;

  function Detect_country return kb_country is

    type kind_of_keyb is ( none, MS, DR, unknown );
    
    -- MS: Microsoft format (MS-DOS & MS Windows)
    -- DR: Digital Research format (Novell-Caldera-Lineo DR-DOS)
    
    regs: dpmi_regs;
    procedure Dosmemget(Offset : in Unsigned_32;
                        Length : in Unsigned_32;
                        Buffer : in out string);
    pragma Import(C, Dosmemget, "dosmemget");
    MS_tmp: string(1..2);

    function Detect_Keyb_API return kind_of_keyb is
      begin
        regs.ax := 16#AD80#;
        regs.bx := 16#Ada#; -- modified by MS-KEYB
        regs.cx := 16#Ada#; -- modified by DR-KEYB
  
        dpmi_int( 16#2F#, regs); -- Multiplex interrupt
  
        if (regs.ax and 16#FF#) /= 16#FF# then  return none;
        elsif regs.bx /= 16#Ada#          then  return MS;
        elsif regs.cx /= 16#Ada#          then  return DR;
        else                                    return unknown;
        end if;

      end;

    begin
      case Detect_Keyb_API is
        when none| unknown => return US; -- no political understatement...
        when DR => return kb_country'val(regs.cx and 16#FF#);
                   -- The kb_country enumerated type matches excactly this
        when MS => 
              dosmemget(
               Offset => unsigned_32(regs.es) * 16 +
                         unsigned_32(regs.di) + 16#16#,
               Length => MS_tmp'Length,
               Buffer => MS_tmp); -- Get the 2 letters for country (US,SF,...)
              return kb_country'value(MS_tmp); -- 'value converts into value
      end case;
    end;

  type updn is array ( boolean ) of character;
  type national_country_table is array( key_value ) of updn;

  kb_table: array( kb_country ) of national_country_table :=
    -- begin with US keyboard for everybody:
  ( others =>
   (  1=> (ESC,ESC),  2=> ('1','!'),  3=> ('2','@'),  4=> ('3','#'),
      5=> ('4','$'),  6=> ('5','%'),  7=> ('6','^'),  8=> ('7','&'),
      9=> ('8','*'), 10=> ('9','('), 11=> ('0',')'), 12=> ('-','_'),
     13=> ('=','+'),
     14=> (BS ,BS ), 15=> (HT ,HT ),
     16=> ('q','Q'), 17=> ('w','W'), 18=> ('e','E'), 19=> ('r','R'),
     20=> ('t','T'), 21=> ('y','Y'), 22=> ('u','U'), 23=> ('i','I'),
     24=> ('o','O'), 25=> ('p','P'), 26=> ('[','{'), 27=> (']','}'),
     28=> (CR, CR ),
     30=> ('a','A'), 31=> ('s','S'), 32=> ('d','D'), 33=> ('f','F'),
     34=> ('g','G'), 35=> ('h','H'), 36=> ('j','J'), 37=> ('k','K'),
     38=> ('l','L'), 39=> (';',':'), 40=> (''','"'), 43=> ('\','|'),
     41=> ('`','~'),
     86=> ('\','|'),
     44=> ('z','Z'), 45=> ('x','X'), 46=> ('c','C'), 47=> ('v','V'),
     48=> ('b','B'), 49=> ('n','N'), 50=> ('m','M'), 51=> (',','<'),
     52=> ('.','>'), 53=> ('/','?'),
     57=> (' ',' '),
     others => (ASCII.NUL,ASCII.NUL) )
  );

  function Key_ASCII ( scancode: key_value; upper: boolean ) return character is
    begin
      return kb_table( country ) ( scancode ) ( upper );
    end Key_ASCII;
    
  function Key_image ( scancode: key_value; upper: boolean ) return string is
    c: character:= Key_ASCII ( scancode, upper );
    begin
      case c is
        when ' ' => return "Space";
        when NUL =>
        case scancode is
          when key_left   => return "Left";
          when key_right  => return "Right";
          when key_up     => return "Up";
          when key_down   => return "Down";
          when key_pgdn   => return "PgDn";
          when key_pgup   => return "PgUp";
          when key_home   => return "Home";
          when key_end    => return "End";

          when key_gray_plus   => return "Gray +";
          when key_gray_minus  => return "Gray -";
          when key_gray_star   => return "Gray *";
          when key_sys_req     => return "SysReq";
          when key_scroll_lock => return "ScrLck";

          when key_F1     => return "F1";
          when key_F2     => return "F2";
          when key_F3     => return "F3";
          when key_F4     => return "F4";
          when key_F5     => return "F5";
          when key_F6     => return "F6";
          when key_F7     => return "F7";
          when key_F8     => return "F8";
          when key_F9     => return "F9";
          when key_F10    => return "F10";
          when key_F11    => return "F11";
          when key_F12    => return "F12";

          when key_ins    => return "Ins";
          when key_del    => return "Del";
          when key_ctrl   => return "Ctrl";
          when key_alt    => return "Alt";
          when key_lshft|
               key_rshft  => return "Shift";
          when key_caps   => return "Caps";
          when others     => return "NUL";
        end case;
      when ASCII.SOH .. ASCII.US =>
        return character'image(c);
      when others =>
        return (1=> c);
      end case;
    end Key_image;

  free: array(key_value) of boolean:= (others=> false);

  function Strike_1 ( scancode: key_value ) return boolean is
    begin
      if keyboard( scancode ) then
        if free( scancode ) then
          free( scancode ):= false; -- key has been recorded as pressed
          return true;
        else
          return false; -- already reported strike
        end if;
      else
        free( scancode ):= true; -- unpressed -> next strike allowed
        return false;
      end if;
    end Strike_1;
begin
  country:= Detect_country;

  -- Now, we customise the US keyboard for each "country"
  --
  -- *=done:   (*US, FR, GR, UK, DK, SV, SU, IT, SP, NO, PO, 
  --             BE, NL, CF, LA,*SF,*SG, RU, TQ, TF, HU, BR);

  kb_table(SF) (2..13):=
    ( ('1','+'), ('2','"'), ('3','*'), ('4','á'), ('5','%'), ('6','&'),
      ('7','/'), ('8','('), ('9',')'), ('0','='), (''','?'), ('^','`') );
  kb_table(SF) (21) := ('z','Z');
  kb_table(SF) (26..27):= ( ('ä','Å'), ('"','!') );
  kb_table(SF) (39..41):= ( ('Ç','î'), ('Ö','Ñ'), ('ı','¯') );
  kb_table(SF) (43):= ('$','ú');
  kb_table(SF) (86):= ('<','>');
  kb_table(SF) (44):= ('y','Y');
  kb_table(SF) (51..53):= ( (',',';'), ('.',':'), ('-','_') );

  kb_table(SG):= kb_table(SF);
  kb_table(SG) (26):= ( ('Å','ä') );
  kb_table(SG) (39..40):= ( ('î','Ç'), ('Ñ','Ö') );

end Multi_keys;
