with Ada.Real_Time;

package body Timer is -- Annex D version
   use Ada.Real_Time;

   Start_Time : Time;

   -----------
   -- Clock --
   -----------

   function Clock return Duration is
   begin
      return To_Duration (Clock - Start_Time);
   end Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      Start_Time := Clock;
   end Reset;
begin
   Reset;
end Timer;
