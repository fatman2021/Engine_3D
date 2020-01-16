-----------------------------------------------------------------------------
--  File: gamedriv.adb; see specification (gamedriv.ads)
-----------------------------------------------------------------------------

package body Game_driving is

  procedure Record_commands(
              c          : in out command_set; -- commands are added to c
              centre_x,
              centre_y,
              size_x,
              size_y     : in  Integer; -- screen dimensions for mouse
              gx,gy      : out Float    -- mouse movement since last call
            ) is

--    use Multi_Keys, PC_Mouse;
    use Keyboard, Mouse;

--    btns: Mouse_button_bunch;
    btns: Press_Array;
    mx,my: integer;
    glisse: boolean:= false;
    sensib: constant:= 8.0;
    key : Character := Keyboard.Get_Key;
    Spare : Boolean;
    begin
     -- Clavier:
--     c(turn_up):=        keyboard( 30 );
--     c(turn_down):=      keyboard( 44 );
--     c(swing_plus):=     keyboard( 32 );
--     c(swing_minus):=    keyboard( 31 );
--     c(photo):=          Strike_1( 45 );
--     c(turn_left):=      keyboard( key_left );
--     c(turn_right):=     keyboard( key_right );
--     c(go_forward):=     keyboard( key_up );
--     c(go_backwards):=   keyboard( key_down );
--     c(interrupt_game):= keyboard( key_esc );
     c(turn_up):=        key = 'u';
     c(turn_down):=      key = 'd';
     c(swing_plus):=     key = '+';
     c(swing_minus):=    key = '-';
     c(photo):=          key = 'p';
     c(turn_left):=      key = 'l';
     c(turn_right):=     key = 'r';
     c(go_forward):=     key = 'f';
     c(go_backwards):=   key = 'b';
     c(interrupt_game):= key = 'q';

     -- Souris:
--     if MouseInstalled then
--      Mouse( mx,my, btns );
      Spare := Mouse.Update_State;
      Mouse.Get_State (mx, my, btns);
      if btns( left )  then c(go_forward):= true; end if;
      if btns( right ) then glisse:= true; end if;
      -- contr“le par mouvement de la souris
      mx:= mx - centre_x;
      my:= my - centre_y;
      if mx/=0 or else my/=0 then
        if mx/=0 then
         gx:= sensib * float(mx)/float(size_x);
         if glisse then
          c(translate_lateral_graduated):= true;
         else
          c(turn_lateral_graduated):= true;
         end if;
        end if;
        if my/=0 then
         gy:= -sensib * float(my)/float(size_y);
         c(go_graduated):= true;
        end if;
--QQ        SetPosition( centre_x, centre_y ); -- remettre au centre la souris
      end if;

--     end if;

    end Record_commands;

end Game_driving;
