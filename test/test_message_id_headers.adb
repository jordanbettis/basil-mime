--
--                 Test Suite for The Basil Library
--
-- Test Cases for message-id-related functionality.
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
with Basil.Message_ID_Headers;       use Basil.Message_ID_Headers;
with Basil.Message_ID_Headers.Lists; use Basil.Message_ID_Headers.Lists;

use Basil;

package body Test_Message_ID_Headers is

   procedure Check_Generate_Message_ID
     (T : in out Aunit.Test_Cases.Test_Case'Class) is

      Test1 : Message_ID_Header;
      Test2 : Message_ID_Header;

      --  It would be infeasably difficult to actually test the
      --  randomness of the system, so we'll just test to make sure
      --  that it's running the random number device *at all*.

   begin -- Check_Generate_Message_ID

      Test1 := Generate_Message_ID ("Message-ID");
      Test2 := Generate_Message_ID ("Message-ID");

      Assert (Get_Message_ID (Test1) /= Get_Message_ID (Test2),
              "Generated message ID randomness check failed");

      Assert (Get_Domain_Part (Test1) /= "",
              "Domain part of generated message ID is empty.");

   end Check_Generate_Message_ID;

   ----------------------------------------------------------------------------
   procedure Check_Message_ID_Input
     (T : in out Aunit.Test_Cases.Test_Case'Class) is

      Test_List    : Message_ID_Headers.Lists.List;
      Test_Cursor  : Message_ID_Headers.Lists.Cursor;
      Test_Headers : Headers.Lists.List;

   begin -- Check_Message_ID_Input

      Append (Test_Headers, To_Header
              ("In-Reply-To",
               "<bcd\@@foo> <555.?234?@ex.com> "
               & "<bar@[12.\[12\]]><\ \ .t@[\ \ \ ]>"));

      Append (Test_Headers, To_Header
              ("References",
               "< ffff @ t . t . t . t . t > < x @ [\*] >"));

      Append (Test_Headers, To_Header
              ("Message-ID",  "  <jordanb@hafd.org> "));

      Test_List := Parse (Test_Headers);

      Assert (Length (Test_List) = 7, "Test List Length Incorrect");

      Test_Cursor := First (Test_List);

      Assert (Get_Key (Element (Test_Cursor)) = "In-Reply-To",
              "Bad Key for First Message ID");
      Assert (Get_Local_Part (Element (Test_Cursor)) = "bcd@",
              "Bad Local Part for First Message ID");
      Assert (Get_Domain_Part (Element (Test_Cursor)) = "foo",
              "Bad Domain Part for First Message ID");
      Assert (Get_Message_ID (Element (Test_Cursor)) = "<bcd\@@foo>",
              "Bad Message-ID for First Message ID");

      Next (Test_Cursor);

      Assert (Get_Key (Element (Test_Cursor)) = "In-Reply-To",
              "Bad Key for Second Message ID");
      Assert (Get_Local_Part (Element (Test_Cursor)) = "555.?234?",
              "Bad Local Part for Second Message ID");
      Assert (Get_Domain_Part (Element (Test_Cursor)) = "ex.com",
              "Bad Domain Part for Second Message ID");
      Assert (Get_Message_ID (Element (Test_Cursor)) = "<555.?234?@ex.com>",
              "Bad Message-ID for Second Message ID");

      Next (Test_Cursor);

      Assert (Get_Key (Element (Test_Cursor)) = "In-Reply-To",
              "Bad Key for First Message ID");
      Assert (Get_Local_Part (Element (Test_Cursor)) = "bar",
              "Bad Local Part for Third Message ID");
      Assert (Get_Domain_Part (Element (Test_Cursor)) = "[12.[12]]",
              "Bad Domain Part for Third Message ID");
      Assert (Get_Message_ID (Element (Test_Cursor)) = "<bar@[12.\[12\]]>",
              "Bad Message-ID for Third Message ID");

      Next (Test_Cursor);

      Assert (Get_Key (Element (Test_Cursor)) = "In-Reply-To",
              "Bad Key for Second Message ID");
      Assert (Get_Local_Part (Element (Test_Cursor)) = "  .t",
              "Bad Local Part for Fourth Message ID");
      Assert (Get_Domain_Part (Element (Test_Cursor)) = "[   ]",
              "Bad Domain Part for Fourth Message ID");
      Assert (Get_Message_ID (Element (Test_Cursor)) = "<\ \ .t@[\ \ \ ]>",
              "Bad Message-ID for Fourth Message ID");


      Next (Test_Cursor);

      Assert (Get_Key (Element (Test_Cursor)) = "References",
              "Bad Key for Fifth Message ID");
      Assert (Get_Local_Part (Element (Test_Cursor)) = "ffff",
              "Bad Local Part for Fifth Message ID");
      Assert (Get_Domain_Part (Element (Test_Cursor)) = "t.t.t.t.t",
              "Bad Domain Part for Fifth Message ID");
      Assert (Get_Message_ID (Element (Test_Cursor)) = "<ffff@t.t.t.t.t>",
              "Bad Message-ID for Fifth Message ID");

      Next (Test_Cursor);

      Assert (Get_Key (Element (Test_Cursor)) = "References",
              "Bad Key for Sixth Message ID");
      Assert (Get_Local_Part (Element (Test_Cursor)) = "x",
              "Bad Local Part for Sixth Message ID");
      Assert (Get_Domain_Part (Element (Test_Cursor)) = "[*]",
              "Bad Domain Part for Sixth Message ID");
      Assert (Get_Message_ID (Element (Test_Cursor)) = "<x@[*]>",
              "Bad Message-ID for Sixth Message ID");

      Next (Test_Cursor);

      Assert (Get_Key (Element (Test_Cursor)) = "Message-ID",
              "Bad Key for Seventh Message ID");
      Assert (Get_Local_Part (Element (Test_Cursor)) = "jordanb",
              "Bad Local Part for Seventh Message ID");
      Assert (Get_Domain_Part (Element (Test_Cursor)) = "hafd.org",
              "Bad Domain Part for Seventh Message ID");
      Assert (Get_Message_ID (Element (Test_Cursor)) = "<jordanb@hafd.org>",
              "Bad Message-ID for Seventh Message ID");

   end Check_Message_ID_Input;

   ----------------------------------------------------------------------------
   procedure Check_Message_ID_Output
     (T : in out Aunit.Test_Cases.Test_Case'Class) is

      IDs          : Message_ID_Headers.Lists.List;
      Test_Headers : Headers.Lists.List;
      Test_Cursor  : Headers.Lists.Cursor;

   begin -- Check_Message_ID_Output

      Append (IDs, To_Message_ID_Header ("In-Reply-To", "ran@dom", "value.d"));
      Append (IDs, To_Message_ID_Header ("References", "bar", "[ [[]] ]"));
      Append (IDs, To_Message_ID_Header ("Message-ID", "jordan", "Bettis"));
      Append (IDs, To_Message_ID_Header ("IN-REPLY-TO", "Test", "shangra.la"));

      Test_Headers := Compile (IDs);
      Test_Cursor  := First (Test_Headers);

      Assert (Length (Test_Headers) = 3,
              "Test Headers Length Incorrect.");

      Assert (Get_Key (Element (Test_Cursor)) = "In-Reply-To",
              "Wrong key for first header.");
      Assert (Get_Value (Element (Test_Cursor)) =
              "<ran\@dom@value.d> <Test@shangra.la> ",
              "Wrong value for first header");

      Next (Test_Cursor);

      Assert (Get_Key (Element (Test_Cursor)) = "Message-ID",
              "Wrong key for first header.");
      Assert (Get_Value (Element (Test_Cursor)) =
              "<jordan@Bettis> ",
              "Wrong value for second header");

      Next (Test_Cursor);

      Assert (Get_Key (Element (Test_Cursor)) = "References",
              "Wrong key for third header.");
      Assert (Get_Value (Element (Test_Cursor)) =
              "<bar@[\ \[\[\]\]\ ]> ",
              "Wrong value for first header");

   end Check_Message_ID_Output;

   ----------------------------------------------------------------------------
   procedure Register_Tests (T : in out Test_Case) is

   begin -- Register_Tests

      Register_Routine (T, Check_Generate_Message_ID'Access,
                        "Test Message ID Generation");
      Register_Routine (T, Check_Message_ID_Input'Access,
                        "Test Message-ID Headers Parsing");
      Register_Routine (T, Check_Message_ID_Output'Access,
                        "Test Message-ID Headers Serialization");

   end Register_Tests;

   ----------------------------------------------------------------------------
   function Name (T : Test_Case) return String_Access is

   begin -- Name

      return new String'("Test Message ID Headers");

   end Name;


end Test_Message_ID_Headers;
