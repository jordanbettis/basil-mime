--
--                 Test Suite for The Basil Library
--
-- Common utilities for the test suite.

with Ada.Characters.Latin_1;

with Basil.Strings;

with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

with Ada.Text_IO;              use Ada.Text_IO;

package body Tests_Common is

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Replace_All (Source_String : in out Unbounded_String;
                          Target        : in     String;
                          Replacement   : in     String) is

      Index_Of_Pattern : Integer;

   begin -- Replace_All

      Index_Of_Pattern := Index( Source_String, Target);

      while Index_Of_Pattern /= 0 loop

         Replace_Slice (Source_String,
                        Index_Of_Pattern,
                        (Index_Of_Pattern + Target'Length - 1),
                        Replacement);
         Index_Of_Pattern := Index( Source_String, Target);

      end loop;

   end Replace_All;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Purge_Newlines (Source : in out Unbounded_String) is

      package L renames Ada.Characters.Latin_1;

   begin -- Purge_Newlines

      Replace_All (Source, L.CR & L.LF, "");

   end Purge_Newlines;

   -----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Purge_Newlines (Source : in String) return String is

      Output : Unbounded_String := US (Source);

   begin -- Purge_Newlines

      Purge_Newlines (Output);

      return S (Output);

   end Purge_Newlines;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Fetch_Entity (Name : in String) return String is

      Entity_File : File_Type;
      Output      : Unbounded_String;
      Char        : Standard.Character;

   begin -- Fetch_Entity

      Open (File => Entity_File,
            Mode => In_File,
            Name => "test/entities/" & Name & ".txt");

      while not End_Of_File (Entity_File) loop

         Get_Immediate (File => Entity_File,
                        Item => Char);

         Append (Output, Char);

      end loop;

      Close (Entity_File);

      return S (Output);

   end Fetch_Entity;

end Tests_Common;
