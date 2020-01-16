with Text_IO; use Text_IO;

with Demo_3D_00, Demo_3D_01;
--  Of course you can also build them separately as main programs

procedure Demos is
  package IIO is new Integer_IO(integer);  use IIO;
  n:  integer;
  ok: boolean;
begin
  loop
    Put_Line( "+------------------------+");
    Put_Line( "| Shell for the 3D demos |");
    Put_Line( "+------------------------+");
    New_Line;
    Put_Line( "Which demo would you like to run ?" );
    New_Line;
    Put_Line( " Demo_3D_00:   rotating objects ...... 0" );
    Put_Line( " Demo_3D_01:   mini 3D game .......... 1" );
    New_Line;
    Put_Line( " Quit ................................ 9" );
    ok:= true;
    begin
      Get( n ); Skip_Line;
      case n is
        when      0 => Demo_3D_00;
        when      1 => Demo_3D_01;
        when      9 => exit;
        when others => Put_Line( "Wrong choice!" ); ok:= false;
      end case;
    exception
      when Data_error =>
        Skip_Line; Put_Line("Valid number, please!"); ok:= false;
    end;
    -- exit when ok;
  end loop;
  
end Demos;