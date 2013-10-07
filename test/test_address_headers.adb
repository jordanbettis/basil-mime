--
--                 Test Suite for The Basil Library
--
-- Test Cases for address-header functionality.
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
with Basil.Address_Headers;          use Basil.Address_Headers;
with Basil.Address_Headers.Lists;    use Basil.Address_Headers.Lists;

use Basil;

package body Test_Address_Headers is

   ----------------------------------------------------------------------------
   procedure Check_Address_Input
     (T : in out Aunit.Test_Cases.Test_Case'Class) is

      Test_List    : Address_Headers.Lists.List;
      Test_Cursor  : Address_Headers.Lists.Cursor;
      Test_Headers : Headers.Lists.List;

   begin -- Check_Message_ID_Input

      Append (Test_Headers, To_Header
              ("To",
               "R. T. Test <test@tester>,Foo<bar@org.org>, , , "
               & "Bar Band: <bar@[12.\[12\]]>, noname @ no . net,"
               & " < addr @ spec > ; ""Foo\. Bar"" <good@try.tv>"));

      Append (Test_Headers, To_Header
              ("From", """Group\-2"" :foo@bar, Hihi <hi@goodby>; "
               & "last-one@heh . heh "));

      Append (Test_Headers, To_Header
              ("Obsolete", "Empty:; Jordan Bettis <@foo @[1\[2\]], "
               & "@kremvax : jordanb@hafd.org> "));

      Test_List := Parse (Test_Headers);

      Assert (Length (Test_List) = 10, "Test List Length Incorrect");

      Test_Cursor := First (Test_List);

      Assert (Get_Key (Element (Test_Cursor)) = "To",
              "Bad Key for First Address");
      Assert (Get_Display_Name (Element (Test_Cursor)) = "R. T. Test",
              "Bad Display Name for First Address");
      Assert (Get_Local_Part (Element (Test_Cursor)) = "test",
              "Bad Local Part for First Address");
      Assert (Get_Domain_Part (Element (Test_Cursor)) = "tester",
              "Bad Domain Part for First Address");
      Assert (Get_Address (Element (Test_Cursor)) = "test@tester",
              "Bad Address for First Address");
      Assert (Get_Full_Address (Element (Test_Cursor))
              = """R\. T\. Test"" <test@tester>",
              "Bad Full Address for First Address");
      Assert (Get_Group_Label (Element (Test_Cursor)) = "",
              "Bad Group Label for First Address");

      Next (Test_Cursor);

      Assert (Get_Key (Element (Test_Cursor)) = "To",
              "Bad Key for Second Address");
      Assert (Get_Display_Name (Element (Test_Cursor)) = "Foo",
              "Bad Display Name for Second Address");
      Assert (Get_Local_Part (Element (Test_Cursor)) = "bar",
              "Bad Local Part for Second Address");
      Assert (Get_Domain_Part (Element (Test_Cursor)) = "org.org",
              "Bad Domain Part for Second Address");
      Assert (Get_Address (Element (Test_Cursor)) = "bar@org.org",
              "Bad Address for Second Address");
      Assert (Get_Full_Address (Element (Test_Cursor)) = "Foo <bar@org.org>",
              "Bad Full Address for Second Address");
      Assert (Get_Group_Label (Element (Test_Cursor)) = "",
              "Bad Group Label for Second Address");

      Next (Test_Cursor);

      Assert (Get_Key (Element (Test_Cursor)) = "To",
              "Bad Key for Third Address");
      Assert (Get_Display_Name (Element (Test_Cursor)) = "",
              "Bad Display Name for Second Address");
      Assert (Get_Local_Part (Element (Test_Cursor)) = "bar",
              "Bad Local Part for Third Address");
      Assert (Get_Domain_Part (Element (Test_Cursor)) = "[12.[12]]",
              "Bad Domain Part for Third Address");
      Assert (Get_Address (Element (Test_Cursor)) = "bar@[12.\[12\]]",
              "Bad Address for Third Address");
      Assert (Get_Full_Address (Element (Test_Cursor)) = "<bar@[12.\[12\]]>",
              "Bad Full Address for Third Address");
      Assert (Get_Group_Label (Element (Test_Cursor)) = "Bar Band",
              "Bad Group Label for Third Address");

      Next (Test_Cursor);

      Assert (Get_Key (Element (Test_Cursor)) = "To",
              "Bad Key for Fourth Address");
      Assert (Get_Display_Name (Element (Test_Cursor)) = "",
              "Bad Display Name for First Address");
      Assert (Get_Local_Part (Element (Test_Cursor)) = "noname",
              "Bad Local Part for Fourth Address");
      Assert (Get_Domain_Part (Element (Test_Cursor)) = "no.net",
              "Bad Domain Part for Fourth Address");
      Assert (Get_Address (Element (Test_Cursor)) = "noname@no.net",
              "Bad Address for Fourth Address");
      Assert (Get_Full_Address (Element (Test_Cursor)) = "<noname@no.net>",
              "Bad Full Address for Fourth Address");
      Assert (Get_Group_Label (Element (Test_Cursor)) = "Bar Band",
              "Bad Group Label for Fourth Address");

      Next (Test_Cursor);

      Assert (Get_Key (Element (Test_Cursor)) = "To",
              "Bad Key for Fifth Address");
      Assert (Get_Display_Name (Element (Test_Cursor)) = "",
              "Bad Display Name for First Address");
      Assert (Get_Local_Part (Element (Test_Cursor)) = "addr",
              "Bad Local Part for Fifth Address");
      Assert (Get_Domain_Part (Element (Test_Cursor)) = "spec",
              "Bad Domain Part for Fifth Address");
      Assert (Get_Address (Element (Test_Cursor)) = "addr@spec",
              "Bad Address for Fifth Address");
      Assert (Get_Full_Address (Element (Test_Cursor)) = "<addr@spec>",
              "Bad Full Address for Fifth Address");
      Assert (Get_Group_Label (Element (Test_Cursor)) = "Bar Band",
              "Bad Group Label for Fifth Address");

      Next (Test_Cursor);

      Assert (Get_Key (Element (Test_Cursor)) = "To",
              "Bad Key for Sixth Address");
      Assert (Get_Display_Name (Element (Test_Cursor)) = "Foo. Bar",
              "Bad Display Name for First Address");
      Assert (Get_Local_Part (Element (Test_Cursor)) = "good",
              "Bad Local Part for Sixth Address");
      Assert (Get_Domain_Part (Element (Test_Cursor)) = "try.tv",
              "Bad Domain Part for Sixth Address");
      Assert (Get_Address (Element (Test_Cursor)) = "good@try.tv",
              "Bad Address for Sixth Address");
      Assert (Get_Full_Address (Element (Test_Cursor)) =
              """Foo\. Bar"" <good@try.tv>",
              "Bad Full Address for Sixth Address");
      Assert (Get_Group_Label (Element (Test_Cursor)) = "",
              "Bad Group Label for Sixth Address");

      Next (Test_Cursor);

      Assert (Get_Key (Element (Test_Cursor)) = "From",
              "Bad Key for Seventh Address");
      Assert (Get_Display_Name (Element (Test_Cursor)) = "",
              "Bad Display Name for First Address");
      Assert (Get_Local_Part (Element (Test_Cursor)) = "foo",
              "Bad Local Part for Seventh Address");
      Assert (Get_Domain_Part (Element (Test_Cursor)) = "bar",
              "Bad Domain Part for Seventh Address");
      Assert (Get_Address (Element (Test_Cursor)) = "foo@bar",
              "Bad Address for Seventh Address");
      Assert (Get_Full_Address (Element (Test_Cursor)) = "<foo@bar>",
              "Bad Full Address for Seventh Address");
      Assert (Get_Group_Label (Element (Test_Cursor)) = "Group-2",
              "Bad Group Label for Seventh Address");

      Next (Test_Cursor);

      Assert (Get_Key (Element (Test_Cursor)) = "From",
              "Bad Key for Eighth Address");
      Assert (Get_Display_Name (Element (Test_Cursor)) = "Hihi",
              "Bad Display Name for First Address");
      Assert (Get_Local_Part (Element (Test_Cursor)) = "hi",
              "Bad Local Part for Eighth Address");
      Assert (Get_Domain_Part (Element (Test_Cursor)) = "goodby",
              "Bad Domain Part for Eighth Address");
      Assert (Get_Address (Element (Test_Cursor)) = "hi@goodby",
              "Bad Address for Eighth Address");
      Assert (Get_Full_Address (Element (Test_Cursor)) = "Hihi <hi@goodby>",
              "Bad Full Address for Eighth Address");
      Assert (Get_Group_Label (Element (Test_Cursor)) = "Group-2",
              "Bad Group Label for Eighth Address");

      Next (Test_Cursor);

      Assert (Get_Key (Element (Test_Cursor)) = "From",
              "Bad Key for Ninth Address");
      Assert (Get_Display_Name (Element (Test_Cursor)) = "",
              "Bad Display Name for First Address");
      Assert (Get_Local_Part (Element (Test_Cursor)) = "last-one",
              "Bad Local Part for Ninth Address");
      Assert (Get_Domain_Part (Element (Test_Cursor)) = "heh.heh",
              "Bad Domain Part for Ninth Address");
      Assert (Get_Address (Element (Test_Cursor)) = "last-one@heh.heh",
              "Bad Address for Ninth Address");
      Assert (Get_Full_Address (Element (Test_Cursor)) = "<last-one@heh.heh>",
              "Bad Full Address for Ninth Address");
      Assert (Get_Group_Label (Element (Test_Cursor)) = "",
              "Bad Group Label for Ninth Address");

      Next (Test_Cursor);

      Assert (Get_Key (Element (Test_Cursor)) = "Obsolete",
              "Bad Key for Tenth Address");
      Assert (Get_Display_Name (Element (Test_Cursor)) = "Jordan Bettis",
              "Bad Display Name for First Address");
      Assert (Get_Local_Part (Element (Test_Cursor)) = "jordanb",
              "Bad Local Part for Tenth Address");
      Assert (Get_Domain_Part (Element (Test_Cursor)) = "hafd.org",
              "Bad Domain Part for Tenth Address");
      Assert (Get_Address (Element (Test_Cursor)) = "jordanb@hafd.org",
              "Bad Address for Tenth Address");
      Assert (Get_Full_Address (Element (Test_Cursor)) =
              "Jordan Bettis <jordanb@hafd.org>",
              "Bad Full Address for Tenth Address");
      Assert (Get_Group_Label (Element (Test_Cursor)) = "",
              "Bad Group Label for Tenth Address");

   end Check_Address_Input;

   ----------------------------------------------------------------------------
   procedure Check_Address_Output
     (T : in out Aunit.Test_Cases.Test_Case'Class) is

      Addrs        : Address_Headers.Lists.List;
      Test_Headers : Headers.Lists.List;
      Test_Cursor  : Headers.Lists.Cursor;

   begin -- Check_Address_Output

      Append (Addrs, To_Address_Header
              ("From", "Joe H. Random", "ran@dom", "value.d"));
      Append (Addrs, To_Address_Header
              ("From", "", "No+name", "[12.[34.56].7]", "The Nameless"));
      Append (Addrs, To_Address_Header
              ("To", "Jordan  Bettis", "jordanb", "hafd.org"));
      Append (Addrs, To_Address_Header
              ("From", "Test", "tester", "shangra.la", "The Nameless"));
      Append (Addrs, To_Address_Header
              ("Cc", "Everyone", "every", "one", "Everybody"));
      Append (Addrs, To_Address_Header
              ("Cc", "", "secret", "agent", "Sec. ret"));
      Append (Addrs, To_Address_Header
              ("From", "", "last", "one.net"));
      Append (Addrs, To_Address_Header
              ("Normal", "Normal", "Plain", "boring.ca"));

      Test_Headers := Compile (Addrs);

      Assert (Length (Test_Headers) = 4,
              "Test Headers Length Incorrect.");

      Test_Cursor  := First (Test_Headers);

      Assert (Get_Key (Element (Test_Cursor)) = "Cc",
              "Wrong key for First header.");
      Assert (Get_Value (Element (Test_Cursor)) =
              "Everybody: Everyone <every@one>; "
              & """Sec\. ret"": <secret@agent>;",
              "Wrong value for First header");

      Next (Test_Cursor);

      Assert (Get_Key (Element (Test_Cursor)) = "From",
              "Wrong key for Second header.");
      Assert (Purge_Newlines (Get_Value (Element (Test_Cursor))) =
              """Joe H\. Random"" <ran\@dom@value.d>, The Nameless: "
              & "<No+name@[12.\[34.56\].7]>, Test <tester@shangra.la>; "
              & "<last@one.net>",
              "Wrong value for Second header");

      Next (Test_Cursor);

      Assert (Get_Key (Element (Test_Cursor)) = "Normal",
              "Wrong key for Third header.");
      Assert (Get_Value (Element (Test_Cursor)) =
              "Normal <Plain@boring.ca>",
              "Wrong value for Third header");

      Next (Test_Cursor);

      Assert (Get_Key (Element (Test_Cursor)) = "To",
              "Wrong key for Fourth header.");
      Assert (Get_Value (Element (Test_Cursor)) =
              "Jordan  Bettis <jordanb@hafd.org>",
              "Wrong value for Fourth header");

   end Check_Address_Output;

   ----------------------------------------------------------------------------
   procedure Register_Tests (T : in out Test_Case) is

   begin -- Register_Tests

      Register_Routine (T, Check_Address_Input'Access,
                        "Test Address Parsing");
      Register_Routine (T, Check_Address_Output'Access,
                        "Test Address Serialization");

   end Register_Tests;

   ----------------------------------------------------------------------------
   function Name (T : Test_Case) return String_Access is

   begin -- Name

      return new String'("Test Address Headers");

   end Name;


end Test_Address_Headers;
