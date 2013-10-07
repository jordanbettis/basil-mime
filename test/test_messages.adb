--
--                 Test Suite for The Basil Library
--
-- Test Cases for MIME entity-related functionality.
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
with Basil.Date_Headers;             use Basil.Date_Headers;
with Basil.Entities.Messages;        use Basil.Entities.Messages;
with Basil.Entities.MIME;            use Basil.Entities.MIME;
with Basil.Content_Types;            use Basil.Content_Types;

use Basil;


package body Test_Messages is

   ----------------------------------------------------------------------------
   procedure Check_Serialization
     (T : in out Aunit.Test_Cases.Test_Case'Class) is

      package L renames Ada.Characters.Latin_1;

      Entity_String : String := Fetch_Entity ("recursion_test");
      Entity_Object : Message;

   begin -- Check_Serialization

      Entity_Object := From_String (Entity_String, 1);

      Assert (To_String (Entity_Object)
              = Ensure_Message_Newlines (Entity_String),
              "Round trip serialization /= original for recursion_test");

      declare

         Rfc_Test : String := Fetch_Entity ("rfc2049_example");

      begin

         Entity_Object := From_String (Rfc_Test);

         Assert (To_String (Entity_Object)'Length = Rfc_Test'Length + 1,
                 "Round trip serialization /= original for rfc2049_example");

      end;

   end Check_Serialization;

   ----------------------------------------------------------------------------
   procedure Check_With_MIME
     (T : in out Aunit.Test_Cases.Test_Case'Class) is

      Object  : Message;
      Current : Child_Cursor;

      Input        : Message;
      Input_Cursor : Child_Cursor;

   begin -- Check_With_MIME

      Object := Compose (From      => To_Address_Header ("From",
                                                         "Jordan Bettis",
                                                         "jordanb",
                                                         "hafd.org"),
                         To        => To_Address_Header ("To",
                                                         "Tester",
                                                         "test",
                                                         "test.org"),
                         Subject   => ES ("This is a test message."),
                         Body_Part => ES ("MIME Entities below."));

      Set_Content_Type (Object,
                        Content_Type'
                        (MIME_Type         => T_Multipart,
                         Multipart_Subtype => ST_Related,
                           Multipart_Delimiter
                           => US (Generate_Multipart_Delimiter),
                         Content_Transfer_Encoding =>
                           CTE_7bit));


      Append_Child
        (Object, To_MIME_Entity (Content_Type'
                                 (MIME_Type         => T_Text,
                                  Text_Subtype      => ST_Plain,
                                  Character_Set     => CS_US_ASCII,
                                  Content_Transfer_Encoding =>
                                    CTE_7bit),
                                 ES ("Last Child")));

      Prepend_Child
        (Object, To_MIME_Entity (Content_Type'
                                 (MIME_Type         => T_Text,
                                  Text_Subtype      => ST_Plain,
                                  Character_Set     => CS_Big5,
                                  Content_Transfer_Encoding =>
                                    CTE_Quoted_Printable),
                                 ES ("Last Child")));

      Current := Last_Child (Object, Strategy_Simple);
      Current := Previous_Child (Current);

      Insert_Child (Parent    => Object,
                    Before    => Current,
                    New_Child => To_MIME_Entity
                      (Content_Type'
                       (MIME_Type         => T_Text,
                        Text_Subtype      => ST_Plain,
                        Character_Set     => CS_ISO_8859_1,
                        Content_Transfer_Encoding =>
                          CTE_Quoted_Printable),
                       ES ("Middle Child(ren)")),
                    Count     => 2);

      Next_Child (Current);
      Next_Child (Current);

      Assert (not Has_Child (Current),
              "Has_Child cursor test not expected");

      Assert (Count_Children (Object) = 4,
              "Count of children /= expected");

      Input := From_String (To_String (Object));

      Assert (Count_Children (Input) = 4,
              "Input child count /= expected");

      Current      := First_Child (Object, Strategy_Simple);
      Input_Cursor := First_Child (Input, Strategy_Simple);

      while Has_Child (Input_Cursor) loop

         Assert (Entities.MIME.Get_Content_Type (Get_Child (Current)) =
                 Entities.MIME.Get_Content_Type (Get_Child (Input_Cursor)),
                 "Input content type /= object content type");

         Next_Child (Current);
         Next_Child (Input_Cursor);

      end loop;

   end Check_With_MIME;

   procedure Check_Send_Rep (T : in out Aunit.Test_Cases.Test_Case'Class) is

      Entity_String : String := Fetch_Entity ("send_rep_test");
      Entity_Object : Message := From_String (Entity_String);

      Senders    : Address_Headers.Lists.List;
      Recipients : Address_Headers.Lists.List;
      Orig_Date  : Date_Headers.Date_Header;

   begin -- Check_Send_Rep

      Senders    := Get_Senders (Entity_Object);
      Recipients := Get_Recipients (Entity_Object);
      Orig_Date  := Get_Creation_Date (Entity_Object);

      Set_Creation_Date (Entity_Object, Orig_Date);

      Assert (Length (Senders) = 2, "Senders list length /= expected");
      Assert (Length (Recipients) = 4, "Recipients list length /= expected");
      Assert (Get_Value (Compile (Get_Creation_Date (Entity_Object)))
              = "Sun, 16 Dec 2007 13:24:38 -0600",
              "Round-trip date /= expected");

   end Check_Send_Rep;

   ----------------------------------------------------------------------------
   procedure Register_Tests (T : in out Test_Case) is

   begin -- Register_Tests

      Register_Routine (T, Check_Serialization'Access,
                        "Test Message Serialization");
      Register_Routine (T, Check_With_MIME'Access,
                        "Test Message Entity with MIME parts");
      Register_Routine (T, Check_Send_Rep'Access,
                        "Test Senders, Recipients and Dates Operations");

   end Register_Tests;

   ----------------------------------------------------------------------------
   function Name (T : Test_Case) return String_Access is

   begin -- Name

      return new String'("Messages");

   end Name;


end Test_Messages;

