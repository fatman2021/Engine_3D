------------------------------------------------------------------------------
--  File:            Gamedriv.ads
--  Description:     Command set for games
--  Date / Version:  25-Dec-1999
--  Author's e-mail: gdemont@hotmail.com
--
--  Copyright (c) Gautier de Montmollin 1999
------------------------------------------------------------------------------

--with Multi_keys, PC_mouse;
with Keyboard;
with Mouse;

package Game_driving is

  type command is (
    go_forward, go_backwards, go_graduated,
    translate_left, translate_right, translate_lateral_graduated,
    turn_left, turn_right, turn_lateral_graduated,
    translate_up, translate_down, translate_vertical_graduated,
    turn_up, turn_down, turn_vertical_graduated,
    swing_plus, swing_minus,
    photo, interrupt_game, bogus_command );

  type command_set is array( command ) of Boolean;
  no_command: constant command_set:= (others=> False);

--  keyboard_command_mapping: array( Multi_keys.key_value ) of command :=
--    ( others=> bogus_command );
  keyboard_command_mapping: array(Character) of command :=
    ( others=> bogus_command );

  -- Record game commands from peripherals (keyboard, mouse) --
  procedure Record_commands(
              c          : in out command_set; -- commands are added to c
              centre_x,
              centre_y,
              size_x,
              size_y     : in  Integer; -- screen dimensions for mouse
              gx,gy      : out Float    -- mouse movement since last call
            );

end Game_driving;
