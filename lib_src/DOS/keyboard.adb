with Multi_Keys;

package body Keyboard is -- DOS
   use Multi_Keys;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      Uninstall;
   end Finalize;

   -------------
   -- Get_Key --
   -------------

   function Get_Key return Character is
   begin
      for Key in key_value loop
         if Strike_1 (Key) then
            return Key_ASCII (Key, False);
         end if;
      end loop;

      return Character'Val (0);
   end Get_Key;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Install;
   end Initialize;

end Keyboard;

