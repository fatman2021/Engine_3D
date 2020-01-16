with Ada.Text_IO;

package body Scanline_profiler is
  count: array( Engine_3D.texture_mapping_mode ) of Natural;

  procedure Initialize is
  begin
    count:= (others=> 0);
  end;

  procedure Add_one_count( m: Engine_3D.texture_mapping_mode ) is
  begin
    count( m ):= count( m ) + 1;
  end;

  procedure Finalize is
  use Ada.Text_IO; package IIO is New Integer_IO(Integer); use IIO;
  f: File_Type;
  t: Natural:= 0;
  begin
    Create(f, name=> "scanline.log" );
    for m in Engine_3D.texture_mapping_mode loop
      t:= t + count(m);
    end loop;
    for m in Engine_3D.texture_mapping_mode loop
      Put( f, count(m), 10);
      Put( f, (count(m)*100)/t, 10);
      Put_Line( f, "%  for " & Engine_3D.texture_mapping_mode'image( m ) );
    end loop;
    Put( f, t, 10); Put_Line(f," -- total");
    Close(f);
  end;

begin
  Initialize;
end Scanline_profiler;
