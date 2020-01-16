with Ada.Finalization;
with Fine_Timer;

package body Timer is -- DOS
   Frequency : constant := 600.0; -- Hz

   Initialized : Boolean := False;

   type Shutdown_Type is new Ada.Finalization.Limited_Controlled
     with null record;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Shutdown_Type);

   procedure Finalize (Object : in out Shutdown_Type) is
   begin
      if Initialized then
         Fine_Timer.Restore_Timer_Frequency;
         Fine_Timer.Uninstall;
         Initialized := False;
      end if;
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      if Initialized = False then
         Fine_Timer.Install;
         Fine_Timer.Set_Timer_Frequency (Frequency);
         Initialized := True;
      end if;
   end Initialize;

   -----------
   -- Clock --
   -----------

   function Clock return Duration is
   begin
      if not Initialized then
         Initialize;
      end if;

      return Duration (Fine_Timer.Counter) / Frequency;
   end Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      if not Initialized then
         Initialize;
      end if;

      Fine_Timer.Counter := 0;
   end Reset;

   Shutdown : Shutdown_Type;
end Timer;
