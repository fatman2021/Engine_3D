with Ada.Finalization;
with Interfaces.C;

package body Keyboard is
   use Interfaces.C;

   Initialized : Boolean := False;

   Waiting_For_Release : array (Key_Code) of Boolean := (others => False);

   procedure keyboard_close;
   pragma Import (C, keyboard_close);

   function keyboard_keypressed (scancode : int) return int;
   pragma Import (C, keyboard_keypressed);

   function keyboard_init return int;
   pragma Import (C, keyboard_init);

   function keyboard_update return int;
   pragma Import (C, keyboard_update);

   ----------------------------
   -- Return_To_Normal_State --
   ----------------------------
   procedure Return_To_Normal_State is
   begin
      keyboard_close;
      Initialized := False;
   end Return_To_Normal_State;

   --------------
   -- Finalize --
   --------------
   type Finalize_Type is new Ada.Finalization.Limited_Controlled with
     null record;

   procedure Finalize (X : in out Finalize_Type);

   procedure Finalize (X : in out Finalize_Type) is
   begin
      Return_To_Normal_State;
   end Finalize;

   Finalizer : Finalize_Type;

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize is
   begin
      if not Initialized then
         if keyboard_init /= 0 then
            raise Keyboard_Error;
         end if;
         Initialized := True;
      end if;
   end Initialize;

   ----------------
   -- Is_Pressed --
   ----------------
   function Is_Pressed (Key : Key_Code) return Boolean is
   begin
      Initialize;
      return keyboard_keypressed (int (Key)) /= 0;
   end Is_Pressed;

   ------------
   -- Update --
   ------------
   procedure Update is
   begin
      Initialize;
      if keyboard_update /= 0 then
         for Key in Key_Code loop
            if Waiting_For_Release (Key) then
               Waiting_For_Release (Key) :=
                 keyboard_keypressed (int (Key)) /= 0;
            end if;
         end loop;
      end if;
   end Update;

   ----------------
   -- Was_Struck --
   ----------------
   function Was_Struck (Key : Key_Code) return Boolean is
   begin
      if Waiting_For_Release (Key) then
         return False;
      else
         Waiting_For_Release (Key) := Is_Pressed (Key);
         return Waiting_For_Release (Key);
      end if;
   end Was_Struck;
end Keyboard;
