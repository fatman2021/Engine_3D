with Graphics.Internals;
with Interfaces;

package body Graphics.Modes is -- DOS
   use Graphics.Internals;
   use Interfaces;

   Mode_Available : array (Mode_Type) of Boolean := (others => False);

   Number_Of_Available_Modes : Natural := 0;

   Got_Available_Modes : Boolean := False;
   
   ---------------------
   -- Available_Modes --
   ---------------------

   function Available_Modes return Mode_Array is
   begin
      if not Got_Available_Modes then
         declare
            Mode_Table   : Unsigned_32;
            Current_Mode : Unsigned_16 := 0;
         begin
            Mode_Table := RM_To_Linear (VideoModePtr);
            while Current_Mode /= 16#FFFF# loop
               Current_Mode := Peek_Word (Integer (Mode_Table));
               for I in Mode_Type loop
                  if Current_Mode = Mode_Type'Enum_Rep (I) and
                    Mode_Available (I) /= True then
                     Mode_Available (I) := True;
                     Number_Of_Available_Modes := Number_Of_Available_Modes + 1;
                  end if;
               end loop;
               Mode_Table := Mode_Table + 2;
            end loop;
         end;
         Got_Available_Modes := True;
      end if;

      declare
         Answer   : Mode_Array (1 .. Number_Of_Available_Modes);
         Position : Positive := 1;
      begin
         for I in Mode_Type loop
            if Mode_Available (I) then
               Answer (Position) := I;
               Position := Position + 1;
            end if;
         end loop;

         return Answer;
      end;
   end Available_Modes;

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height return Positive is
   begin
      return 0; --QQ!
   end Get_Height;

   function Get_Height (Mode : Mode_Type) return Positive is
   begin
      return 0; --QQ!
   end Get_Height;

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width return Positive is
   begin
      return 0; --QQ!
   end Get_Width;

   function Get_Width (Mode : Mode_Type) return Positive is
   begin
      return 0; --QQ!
   end Get_Width;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode (Mode : Mode_Type) is
   begin
      null; --QQ!
   end Set_Mode;

   ---------------
   -- Text_Mode --
   ---------------

   procedure Text_Mode is
   begin
      Set_Text_Mode;
   end Text_Mode;

   ---------------
   -- To_String --
   ---------------

   function To_String (Mode : Mode_Type) return String is
   begin
      return Mode_Type'Image (Mode); --QQ Use table
   end To_String;
end Graphics.Modes;
