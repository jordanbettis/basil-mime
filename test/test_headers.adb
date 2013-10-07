--
--                 Test Suite for The Basil Library
--
-- Test Cases for header-related functionality.
--

with Ada.Strings.Unbounded;          use Ada.Strings.Unbounded;
with Ada.Strings.Maps;               use Ada.Strings.Maps;
with Ada.Containers;                 use Ada.Containers;
with Ada.Strings.Fixed;
with Ada.Characters.Latin_1;

with AUnit.Test_Cases;               use AUnit.Test_Cases;
with AUnit.Test_Cases.Registration;  use AUnit.Test_Cases.Registration;
with AUnit.Assertions;               use AUnit.Assertions;

with Ada.Text_IO;                    use Ada.Text_IO;

with Tests_Common;                   use Tests_Common;

with Basil.Strings;                  use Basil.Strings;
with Basil.Tokens;
with Basil.Headers;                  use Basil.Headers;
with Basil.Headers.Lists;            use Basil.Headers.Lists;

use Basil;


package body Test_Headers is

   ----------------------------------------------------------------------------
   --  Ensure there are no lines longer than 78 characters long;
   function Is_Properly_Wrapped (Header_Value : in String)
                                return Boolean is

      package L renames Ada.Characters.Latin_1;

      Value         : Unbounded_String := US (Header_Value);
      Current_Index : Natural;

   begin -- Is_Properly_Wrapped

      Current_Index :=
        Index (Value, L.CR & L.LF);

      while Current_Index /= 0 loop
         if Current_Index > 78 then
            return False;
         end if;
         Tail (Value, Length (Value) - (Current_Index + 1));
         Current_Index :=
           Index (Value, L.CR & L.LF);
      end loop;

      if Length(Value) > 78 then
         return False;
      end if;

      return True;

   end Is_Properly_Wrapped;

   procedure Check_From_String (T : in out Aunit.Test_Cases.Test_Case'Class) is

      package L renames Ada.Characters.Latin_1;

      CRLF     : String := L.CR & L.LF;

      F_Value : String := "From this assyrian Garden, where the Fiend" & CRLF &
        " Saw undelighted all delight, all kind" & CRLF &
        " Of living Creatures new to sight and strange:" & CRLF &
        " To of far nobler shape erect and tall,  " & CRLF &
        " Godlike erect, with native Honour clad" & CRLF &
        " In naked Majestie seemd Lords of all, " & CRLF &
        " And worthie seemd, for in thir looks Divine" & CRLF &
        " The image of thir glorious Maker shon," & CRLF &
        " Truth, Wisdom, Sanctitude sever and pure," & CRLF &
        " Severe, but in true filial freedom plac't;" & CRLF;
      UF_Header      : Header := From_String ( "Milton:" & F_Value);
      Empty_Value    : Header := From_String ( "Empty~Value:" & CRLF);
      Empty_Header   : Header := From_String ( L.CR & L.LF);

      Two_Headers    : String := "Some: Say the world" & CRLF &
        " will end in fire; others favor ice" & CRLF &
        "From: what I've tasted of desire I hold with those who favor fire" &
        CRLF & CRLF;
      TH_Position    : Positive := 1;

      Scratch_String : Unbounded_String := US(F_Value);
      Scratch_Header : Header;

   begin -- Check_From_String

      Assert (Get_Key(UF_Header) = "Milton", "Header.Key /= Header Key");
      Assert ((Get_Value(UF_Header) & CRLF) = F_Value,
              "Header.Value /= Header Value");
      Replace_All (Scratch_String,
                   L.CR & L.LF,
                   "" & L.CR);
      Assert ((Get_Value
               (From_String ("Milton: " & S (Scratch_String))) & L.CR)
              = S (Scratch_String),
              "Header.Value /= Header Value with lone control");
      Replace_All (Scratch_String,
                   "" & L.CR,
                   "" & L.LF);
      Assert ((Get_Value
               (From_String ("Milton: " & S (Scratch_String))) & L.LF)
              = S (Scratch_String),
              "Header.Value /= Header Value with lone linefeed");
      Assert (Get_Key(Empty_Value) = "Empty~Value",
              "Empty_Value.Key /= Empty Value Key");
      Assert (Get_Value(Empty_Value) = "",
              "Empty_Value.Value /= empty string");
      Assert (Get_Key(Empty_Header) = "",
              "Empty_Header.Key /= empty string");
      Assert (Get_Value(Empty_Header) = "",
              "Empty_Header.Value /= empty string");
      From_String(Scratch_Header,
                  Two_Headers,
                  TH_Position);
      Assert(TH_Position = 59, "Index position 1 incorrectly updated: " &
             TH_Position'Img);
      Assert(Get_Key(Scratch_Header) = "Some",
             "Two_Header first key incorrect");
      From_String(Scratch_Header,
                  Two_Headers,
                  TH_Position);
      Assert(TH_Position = 126, "Index position 2 incorrectly updated: " &
             TH_Position'Img);
      Assert(Get_Key(Scratch_Header) = "From",
             "Two_Header second key incorrect");
      From_String(Scratch_Header,
                  Two_Headers,
                  TH_Position);
      Assert(TH_Position = 128, "Index position 3 incorrectly updated: " &
             TH_Position'Img);
      Assert(Get_Key(Scratch_Header) = "",
             "Two_Header end key not empty");
      Assert(Get_Value(Scratch_Header) = "",
             "Two_Header end value not empty");

   end Check_From_String;

   ----------------------------------------------------------------------------
   --  Test proper handling and recovery from bad input to From_String
   procedure Check_From_String_Badness
     (T : in out Aunit.Test_Cases.Test_Case'Class) is

      package L renames Ada.Characters.Latin_1;

      Scratch      : Header;
      Error_Caught : Boolean := False;

   begin -- Check_From_String_Badness

      begin
         Scratch := From_String ("Invalid: No Termination");
      exception
         when Invalid_Header =>
            Error_Caught := True;
      end;
      Assert (Error_Caught, "Did not catch no termination");
      Error_Caught := False;

      begin
         Scratch := From_String ("Invalid Key: Value" & L.CR & L.LF);
      exception
         when Invalid_Header =>
            Error_Caught := True;
      end;
      Assert (Error_Caught, "Did not catch invalid key");


   end Check_From_String_Badness;

   ----------------------------------------------------------------------------
   --  Test header object creation and manipulation functions
   procedure Check_Header_Creation
     (T : in out AUnit.Test_Cases.Test_Case'Class) is

      package L renames Ada.Characters.Latin_1;

      Long_Value : String :=
        "TeX does not treat / as a binary operation, even " &
        "though a slash stands for division (which qualifies as a binary " &
        "operation on mathematical grounds). The reason is that printers " &
        "traditionally put extra space around the symbols +, -, and * but " &
        "not around /. If TeX were to typeset / as a binary operation, the " &
        "formula `$1/2$' would come out as `1 / 2', which is wrong; so TeX " &
        "considers / to be an ordinary symbol.";
      Long_Word  : String := S (512 * 'a');

      Long_Header  : Header := To_Header (ES ("Long_Header"), ES (Long_Value));
      Empty_Value  : Header := To_Header (ES ("Empty"), ES(""));
      VLong_Header : Header := To_Header (ES ("V*Long"), ES (Long_Word));

      Scratch_Header : Header            := Empty_Value;
      Scratch_String : Unbounded_String;

   begin -- Check_Header_Creation

      Assert (Is_Properly_Wrapped (To_String (Long_Header)),
              "Long_Header improperly wrapped");
      Assert (To_String (Long_Header)(1 .. 16) = "Long_Header: TeX",
              "Long_Header serialized head inproperly constructed");
      Scratch_String := US (To_String (Long_Header));
      Tail (Scratch_String, 9);
      Assert (Scratch_String = "symbol." & L.CR & L.LF,
              "Long_Header serialized tail improperly constructed");
      Assert (To_String (Empty_Value) = "Empty: " & L.CR & L.LF,
              "Empty_Value serialized value improperly constructed");
      Set_Key   (Scratch_Header, ES ("Long_Header"));
      Set_Value (Scratch_Header, ES (Long_Value));
      Assert (To_String (Scratch_Header) = To_String (Long_Header),
              "Scratch with set key/value /= Long_Header");
      -- This should not be folded at all as there's no good breakpoint
      Assert (To_String (Vlong_Header) = "V*Long: " & Long_Word & L.CR & L.LF,
              "Very long word header improperly constructed");

   end Check_Header_Creation;

   ----------------------------------------------------------------------------
   --  Test proper handling and recovery from bad input to header creation
   --  functions
   procedure Check_Header_Creation_Badness
     (T : in out Aunit.Test_Cases.Test_Case'Class) is

      package L renames Ada.Characters.Latin_1;

      Scratch      : Header := To_Header ("Prime", "Value");
      Error_Caught : Boolean := False;

   begin -- Check_Quoted_Printable_Badness

      begin
         Scratch := To_Header ("Invalid:", "Broken");
      exception
         when Invalid_Character =>
            Error_Caught := True;
      end;
      Assert (Error_Caught, "To_Header Did not catch ':' in key");
      Error_Caught := False;

      begin
         Scratch := To_Header ("Bad Key", "Broken");
      exception
         when Invalid_Character =>
            Error_Caught := True;
      end;
      Assert (Error_Caught,
              "To_Header Did not catch illegal character in key");
      Error_Caught := False;

      begin
         Set_Key (Scratch, "Invalid:");
      exception
         when Invalid_Character =>
            Error_Caught := True;
      end;
      Assert (Error_Caught, "Set_Key Did not catch ':' in key");
      Error_Caught := False;

      begin
         Set_Key (Scratch, "Bad Key");
      exception
         when Invalid_Character =>
            Error_Caught := True;
      end;
      Assert (Error_Caught, "Set_Key Did not catch illegal character in key");
      Error_Caught := False;

      begin
         Scratch := To_Header ("Invalid", "Bad" & L.CR & "Value");
      exception
         when Invalid_Character =>
            Error_Caught := True;
      end;
      Assert (Error_Caught,
              "To_Header did not catch illegal character in value");

      begin
         Set_Value (Scratch, "Bad" & L.LF & "Value");
      exception
         when Invalid_Character =>
            Error_Caught := True;
      end;
      Assert (Error_Caught,
              "Set_Value did not catch illegal character in value");

      begin
         Scratch := To_Header ( ES ("Too_Long"), ES (500 * "Boo "));
      exception
         when Invalid_Header =>
            Error_Caught := True;
      end;
      Assert (Error_Caught,
              "To_Header did not overly long header");

      begin
         Set_Value (Scratch, ES (500 * "Boo "));
      exception
         when Invalid_Header =>
            Error_Caught := True;
      end;
      Assert (Error_Caught,
              "Set_Value did not catch overly long header");

      begin
         Set_Value (Scratch, ES (500 * "Boo "));
      exception
         when Invalid_Header =>
            Error_Caught := True;
      end;
      Assert (Error_Caught,
              "Set_Value did not catch overly long header");

   end Check_Header_Creation_Badness;

   ----------------------------------------------------------------------------
   --  The most important is to test library-defined operations,
   --  rather than those pulled in from the Container
   procedure Check_Header_Lists
     (T : in out Aunit.Test_Cases.Test_Case'Class) is

      Scratch : Headers.Lists.List;

   begin -- Check_Header_Lists

      Ensure_Header_Exists (Scratch, "Test", "Value");

      Assert (Length (Scratch) = 1,
              "Length of list /= 1 after first call to Ensure_Header_Exists");

      Ensure_Header_Exists (Scratch, To_Header ("Test", "New Value"));

      Assert (Length (Scratch) = 1,
              "Length of list /= 1 after second Ensure_Header_Exists call");

      Assert (Get_Value (First_Element (Scratch)) = "New Value",
              "Ensure_Header_Exists value check failed");

      Prepend (Scratch, To_Header ("First", "Header"));
      Append  (Scratch, To_Header ("Middle", "Header"));
      Append  (Scratch, To_Header ("Test", "Another Deletable"));
      Append  (Scratch, To_Header ("Test", "Final"));

      Assert (Length (Scratch) = 5,
              "Length of list /= 5 after appends and prepends");

      Purge_Header (Scratch, "Test");

      Assert (Length (Scratch) = 2,
              "Length of list /= 2 after purge of 'Test' headers");

      Purge_Header (Scratch, "First");
      Purge_Header (Scratch, "Last");

      Assert (Scratch = Headers.Lists.Empty_List,
             "Scratch /= Empty_List after all purges");

   end Check_Header_Lists;

   ----------------------------------------------------------------------------
   procedure Register_Tests (T : in out Test_Case) is

   begin -- Register_Tests

      Register_Routine (T, Check_From_String'Access,
                        "Test Header Line Parsing");
      Register_Routine (T, Check_From_String_Badness'Access,
                        "Test Header Line Parsing Bad Input");
      Register_Routine (T, Check_Header_Creation'Access,
                        "Test Header Creation and Serialization");
      Register_Routine (T, Check_Header_Creation_Badness'Access,
                        "Test Header Creation Bad Input");


   end Register_Tests;

   ----------------------------------------------------------------------------
   function Name (T : Test_Case) return String_Access is

   begin -- Name

      return new String'("Test Headers");

   end Name;


end Test_Headers;
