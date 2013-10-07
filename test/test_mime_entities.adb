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
with Basil.Entities.MIME;            use Basil.Entities.MIME;
with Basil.Content_Types;            use Basil.Content_Types;

use Basil;


package body Test_MIME_Entities is

   ----------------------------------------------------------------------------
   procedure Check_Serialization
     (T : in out Aunit.Test_Cases.Test_Case'Class) is

      package L renames Ada.Characters.Latin_1;

      Entity_String : String := Fetch_Entity ("recursion_test");
      Entity_Object : MIME_Entity;

   begin -- Check_Recursion

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
   procedure Check_Misc_Tasks
     (T : in out Aunit.Test_Cases.Test_Case'Class) is

      Object  : MIME_Entity;
      Current : Child_Cursor;

      Input        : MIME_Entity;
      Input_Cursor : Child_Cursor;

   begin -- Check_Misc_Tasks

      Object := To_MIME_Entity (Content_Type'
                                (MIME_Type         => T_Multipart,
                                 Multipart_Subtype => ST_Related,
                                 Multipart_Delimiter
                                   => US (Generate_Multipart_Delimiter),
                                 Content_Transfer_Encoding =>
                                   CTE_8bit),
                                S ("Multipart Parent"));


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

         Assert (Get_Content_Type (Get_Child (Current)) =
                 Get_Content_Type (Get_Child (Input_Cursor)),
                 "Input content type /= object content type");

         Next_Child (Current);
         Next_Child (Input_Cursor);

      end loop;

   end Check_Misc_Tasks;

   ----------------------------------------------------------------------------
   procedure Check_Update_Query_Child
     (T : in out Aunit.Test_Cases.Test_Case'Class) is

      Entity_String : String := Fetch_Entity ("recursion_test");
      Entity_Object : MIME_Entity := From_String (Entity_String);

      Bodies        : Unbounded_String;

      Current       : Child_Cursor := First_Child (Entity_Object,
                                                  Strategy_Recursive);

      procedure Alter   (Item : in out Entities.Abstract_Entity'Class) is

      begin -- Alter

         Entities.Set_Body_Part (Item, ES("x"));

      end Alter;

      procedure Process (Item : in Entities.Abstract_Entity'Class) is

      begin -- Process

         Append (Bodies,
                 US (Purge_Newlines (Get_Body_Part (MIME_Entity(Item)))));

      end Process;

   begin -- Check_Update_Query_Child

      while Has_Child (Current) loop

         Update_Child (Entity_Object, Current, Alter'Access);

         Query_Child (Current, Process'Access);

         Next_Child (Current);

      end loop;

      Assert (S (Bodies) = "xxxxxxxxxx", "Bodies /= Expected");

   end Check_Update_Query_Child;

   ----------------------------------------------------------------------------
   procedure Check_Swap_Children
     (T : in out Aunit.Test_Cases.Test_Case'Class) is

      Target_String : String := Fetch_Entity ("recursion_test");
      Target_Object : MIME_Entity := From_String (Target_String);

      I_Cursor      : Child_Cursor := First_Child (Target_Object,
                                                   Strategy_Simple);
      J_Cursor      : Child_Cursor := Last_Child (Target_Object,
                                                  Strategy_Recursive);

   begin -- Check_Swap_Children

      Swap_Children (Target_Object, I_Cursor, J_Cursor);

      Assert (Purge_Newlines
              (Get_Body_Part (Get_First_Child (Target_Object))) = "j",
              "First child /= expected");

      Assert (Purge_Newlines
              (Get_Body_Part (Get_Last_Child (Target_Object,
                                              Strategy_Simple))) = "h",
              "Last child simple /= expected");

      Assert (Purge_Newlines
              (Get_Body_Part (Get_Last_Child (Target_Object,
                                              Strategy_Recursive))) = "f",
              "Last child recursive /= expected");

   end Check_Swap_Children;

   ----------------------------------------------------------------------------
   procedure Check_Splice_Children
     (T : in out Aunit.Test_Cases.Test_Case'Class) is

      Target_String : String := Fetch_Entity ("recursion_test");
      Target_Object : MIME_Entity := From_String (Target_String);

      Source_String : String := Fetch_Entity ("recursion_test");
      Source_Object : MIME_Entity := From_String (Target_String);

      Current       : Child_Cursor;

   begin -- Check_Splice_Children

      Delete_Last_Child (Target_Object);
      Delete_First_Child (Source_Object, 2);

      Splice_Children (Target => Target_Object,
                       Before => No_Child,
                       Source => Source_Object);


      Assert (Count_Children (Target_Object) = 3,
              "Splice children length not expected.");

      Current := Last_Child (Target_Object, Strategy_Recursive);

      Assert (Purge_Newlines (Get_Body_Part (Get_Child (Current))) = "j",
              "Last child recusrive not 'j'");

      Current := Last_Child (Target_Object, Strategy_Simple);

      Assert (Purge_Newlines (Get_Body_Part (Get_Child (Current))) = "h",
              "Last child simple not 'h'");

      Previous_Child (Current);

      Assert (Purge_Newlines (Get_Body_Part (Get_Child (Current))) = "g",
              "Second to last child simple not 'g'");

   end Check_Splice_Children;

   ----------------------------------------------------------------------------
   procedure Check_Recursion (T : in out Aunit.Test_Cases.Test_Case'Class) is

      Entity_String : String := Fetch_Entity ("recursion_test");
      Entity_Object : MIME_Entity;
      Current       : Entities.Child_Cursor;

   begin -- Check_Recursion

      Entity_Object := From_String (Entity_String);

      Assert (Count_Children (Entity_Object) = 3,
              "Parent child list length incorrect");

      Current := First_Child (Entity_Object, Strategy_Recursive);

      Assert (Purge_Newlines (Get_Body_Part (Get_Child (Current))) = "a",
              "Forward recursive walk test failed for 'a' child");

      Next_Child (Current);

      Assert (Purge_Newlines (Get_Body_Part (Get_Child (Current))) = "b",
              "Forward recursive walk test failed for 'b' child");

      Next_Child (Current);

      Assert (Purge_Newlines (Get_Body_Part (Get_Child (Current))) = "c",
              "Forward recursive walk test failed for 'c' child");

      Next_Child (Current);

      Assert (Purge_Newlines (Get_Body_Part (Get_Child (Current))) = "d",
              "Forward recursive walk test failed for 'd' child");

      Next_Child (Current);

      Assert (Purge_Newlines (Get_Body_Part (Get_Child (Current))) = "e",
              "Forward recursive walk test failed for 'e' child");

      Next_Child (Current);

      Assert (Purge_Newlines (Get_Body_Part (Get_Child (Current))) = "f",
              "Forward recursive walk test failed for 'f' child");

      Next_Child (Current);

      Assert (Purge_Newlines (Get_Body_Part (Get_Child (Current))) = "g",
              "Forward recursive walk test failed for 'g' child");

      Next_Child (Current);

      Assert (Purge_Newlines (Get_Body_Part (Get_Child (Current))) = "h",
              "Forward recursive walk test failed for 'h' child");

      Next_Child (Current);

      Assert (Purge_Newlines (Get_Body_Part (Get_Child (Current))) = "i",
              "Forward recursive walk test failed for 'i' child");

      Next_Child (Current);

      Assert (Purge_Newlines (Get_Body_Part (Get_Child (Current))) = "j",
              "Forward recursive walk test failed for 'j' child");

      Next_Child (Current);

      Assert (Has_Child (Current) = False,
              "Forward recursive walk cursor did not terminate correctly");

      Current := Last_Child (Entity_Object, Strategy_Recursive);

      Assert (Purge_Newlines (Get_Body_Part (Get_Child (Current))) = "j",
              "Reverse recursive walk test failed for 'j' child");

      Previous_Child (Current);

      Assert (Purge_Newlines (Get_Body_Part (Get_Child (Current))) = "i",
              "Reverse recursive walk test failed for 'i' child");

      Previous_Child (Current);

      Assert (Purge_Newlines (Get_Body_Part (Get_Child (Current))) = "h",
              "Reverse recursive walk test failed for 'h' child");

      Previous_Child (Current);

      Assert (Purge_Newlines (Get_Body_Part (Get_Child (Current))) = "g",
              "Reverse recursive walk test failed for 'g' child");

      Previous_Child (Current);

      Assert (Purge_Newlines (Get_Body_Part (Get_Child (Current))) = "f",
              "Reverse recursive walk test failed for 'f' child");

      Previous_Child (Current);

      Assert (Purge_Newlines (Get_Body_Part (Get_Child (Current))) = "e",
              "Reverse recursive walk test failed for 'e' child");

      Previous_Child (Current);

      Assert (Purge_Newlines (Get_Body_Part (Get_Child (Current))) = "d",
              "Reverse recursive walk test failed for 'd' child");

      Previous_Child (Current);

      Assert (Purge_Newlines (Get_Body_Part (Get_Child (Current))) = "c",
              "Reverse recursive walk test failed for 'c' child");

      Previous_Child (Current);

      Assert (Purge_Newlines (Get_Body_Part (Get_Child (Current))) = "b",
              "Reverse recursive walk test failed for 'b' child");

      Previous_Child (Current);

      Assert (Purge_Newlines (Get_Body_Part (Get_Child (Current))) = "a",
              "Reverse recursive walk test failed for 'a' child");

      Previous_Child (Current);

      Assert (Has_Child (Current) = False,
              "Reverse recursive walk cursor did not terminate correctly");

   end Check_Recursion;

   ----------------------------------------------------------------------------
   procedure Check_Get_Child (T : in out Aunit.Test_Cases.Test_Case'Class) is

      Entity_String : String := Fetch_Entity ("recursion_test");
      Entity_Object : MIME_Entity := From_String (Entity_String);

   begin -- Check_Get_Child

      Assert (Get_Body_Part (Get_First_Child (Entity_Object)) = "a",
              "Get First Child failed with Strategy_Simple");

      Assert (Get_Body_Part
              (Get_Last_Child
               (Entity_Object, Strategy_Simple)) = "h",
              "Get Last Child failed with Strategy_Simple");

      Assert (Get_Body_Part
              (Get_Last_Child
               (Entity_Object, Strategy_Recursive)) = "j",
              "Get First Child failed with Strategy_Recursive");

   end Check_Get_Child;

   ----------------------------------------------------------------------------
   procedure Check_Cursor_Safety
     (T: in out Aunit.Test_Cases.Test_Case'Class) is

      Entity_String : String := Fetch_Entity ("recursion_test");
      Entity_Object : MIME_Entity := From_String (Entity_String);

      Canary        : Child_Cursor := First_Child (Entity_Object,
                                                   Strategy_Recursive);
      Deleter       : Child_Cursor := First_Child (Entity_Object,
                                                   Strategy_Simple);
      Scratch       : MIME_Entity;
      Error_Caught  : Boolean;

   begin -- Check_Cursor_Safety

      Next_Child (Canary);
      Next_Child (Canary);
      Next_Child (Canary);

      Delete_Child (Entity_Object, Deleter);

      Assert (not Has_Child (Deleter), "Deleter /= No_Child");

      begin
         Next_Child (Canary);
      exception
         when others =>
            Error_Caught := True;
      end;
      Assert (Error_Caught, "Did Not Catch Illegal Cursor with Next");
      Error_Caught := False;

      begin
         Previous_Child (Canary);
      exception
         when others =>
            Error_Caught := True;
      end;
      Assert (Error_Caught, "Did Not Catch Illegal Cursor with Previous");
      Error_Caught := False;

      begin
         Scratch := Get_Child (Canary);
      exception
         when others =>
            Error_Caught := True;
      end;
      Assert (Error_Caught, "Did Not Catch Illegal Cursor with Get_Child");
      Error_Caught := False;

   end Check_Cursor_Safety;

   ----------------------------------------------------------------------------
   procedure Check_Find_With_Type
     (T : in out Aunit.Test_Cases.Test_Case'Class) is

      Entity_String : String := Fetch_Entity ("rfc2049_example");
      Entity_Object : MIME_Entity := From_String (Entity_String);
      Cursor_Object : Child_Cursor;

   begin -- Check_Find_With_Type

      Cursor_Object :=
        Find_With_Type (Entity_Object,
                        Content_Type'(MIME_Type     => T_Image,
                                      Image_Subtype => ST_JPEG,
                                      Content_Transfer_Encoding => CTE_Base64),
                        No_Child_Recursive);

      Assert (Purge_Newlines (Get_Body_Part (Get_Child (Cursor_Object)))
              = "   ... base64-encoded image data goes here ...",
              "image/jpeg's body /= expected");

   end Check_Find_With_Type;

   ----------------------------------------------------------------------------
   procedure Check_Reverse_Find_With_Type
     (T : in out Aunit.Test_Cases.Test_Case'Class) is

      Entity_String : String := Fetch_Entity ("recursion_test");
      Entity_Object : MIME_Entity := From_String (Entity_String);
      Cursor_Object : Child_Cursor;

   begin -- Check_Reverse_Find_With_Type

      Cursor_Object :=
        Reverse_Find_With_Type
        (Entity_Object,
         Content_Type'(MIME_Type     => T_Text,
                       Text_Subtype  => ST_Enriched,
                       Character_Set => CS_US_ASCII,
                       Content_Transfer_Encoding => CTE_Quoted_Printable),
         No_Child_Recursive);

      Assert (Purge_Newlines (Get_Body_Part (Get_Child (Cursor_Object)))
              = "j", "j's body /= expected");

      Cursor_Object :=
        Reverse_Find_With_Type
        (Entity_Object,
         Content_Type'(MIME_Type     => T_Text,
                       Text_Subtype  => ST_Enriched,
                       Character_Set => CS_US_ASCII,
                       Content_Transfer_Encoding => CTE_Quoted_Printable),
         Previous_Child (Cursor_Object));

      Assert (Purge_Newlines (Get_Body_Part (Get_Child (Cursor_Object)))
              = "e", "e's body /= expected");

   end Check_Reverse_Find_With_Type;

   ----------------------------------------------------------------------------
   procedure Check_Iteration (T : in out Aunit.Test_Cases.Test_Case'Class) is

      Entity_String : String := Fetch_Entity ("recursion_test");
      Entity_Object : MIME_Entity := From_String (Entity_String);

      Bodies        : Unbounded_String;

      procedure Process (Position : Child_Cursor) is

      begin -- Process

         Append (Bodies,
                 US (Purge_Newlines (Get_Body_Part (Get_Child (Position)))));

      end Process;

   begin -- Check_Iteration

      Iterate_Children (Entity_Object, Strategy_Recursive, Process'Access);

      Assert (S (Bodies) = "bdecfagijh", "Bodies /= Expected");

   end Check_Iteration;

   ----------------------------------------------------------------------------
   procedure Check_Iteration_Reverse
     (T : in out Aunit.Test_Cases.Test_Case'Class) is

      Entity_String : String := Fetch_Entity ("recursion_test");
      Entity_Object : MIME_Entity := From_String (Entity_String);

      Bodies        : Unbounded_String;

      procedure Process (Position : Child_Cursor) is

      begin -- Process

         Append (Bodies,
                 US (Purge_Newlines (Get_Body_Part (Get_Child (Position)))));

      end Process;

   begin -- Check_Iteration_Reverse

      Reverse_Iterate_Children
        (Entity_Object, Strategy_Recursive, Process'Access);

      Assert (S (Bodies) = "jihgfedcba", "Bodies /= Expected");

   end Check_Iteration_Reverse;

   ----------------------------------------------------------------------------
   procedure Register_Tests (T : in out Test_Case) is

   begin -- Register_Tests

      Register_Routine (T, Check_Serialization'Access,
                        "Test MIME Entity Serialization");
      Register_Routine (T, Check_Misc_Tasks'Access,
                        "Test Children Management Misc Tasks");
      Register_Routine (T, Check_Update_Query_Child'Access,
                        "Test Child Update and Query Operations");
      Register_Routine (T, Check_Swap_Children'Access,
                        "Test Swap Children");
      Register_Routine (T, Check_Splice_Children'Access,
                        "Test Splice Children");
      Register_Routine (T, Check_Recursion'Access,
                        "Test MIME Entity Recursion");
      Register_Routine (T, Check_Cursor_Safety'Access,
                        "Check Cursor Safety");
      Register_Routine (T, Check_Find_With_Type'Access,
                        "Check Find With Type");
      Register_Routine (T, Check_Reverse_Find_With_Type'Access,
                        "Check Reverse Find With Type");
      Register_Routine (T, Check_Iteration'Access,
                        "Test child Entity Iteration");
      Register_Routine (T, Check_Iteration_Reverse'Access,
                        "Test child Entity Reverse Iteration");

   end Register_Tests;

   ----------------------------------------------------------------------------
   function Name (T : Test_Case) return String_Access is

   begin -- Name

      return new String'("MIME Entities");

   end Name;


end Test_MIME_Entities;

