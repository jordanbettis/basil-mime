--
--                 Test Suite for The Basil Library
--
-- Test Cases for header-related functionality.
--

with Ada.Strings.Unbounded;          use Ada.Strings.Unbounded;
with Ada.Strings.Maps;               use Ada.Strings.Maps;
with Ada.Containers;                 use Ada.Containers;
with Ada.Characters.Handling;        use Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Characters.Latin_1;

with AUnit.Test_Cases;               use AUnit.Test_Cases;
with AUnit.Test_Cases.Registration;  use AUnit.Test_Cases.Registration;
with AUnit.Assertions;               use AUnit.Assertions;

with Ada.Text_IO;                    use Ada.Text_IO;

with Tests_Common;                   use Tests_Common;

with Basil.Strings;                  use Basil.Strings;
with Basil.Headers;                  use Basil.Headers;
with Basil.Headers.Lists;            use Basil.Headers.Lists;
with Basil.Content_Types;            use Basil.Content_Types;

use Basil;


package body Test_Content_Types is

   ----------------------------------------------------------------------------
   procedure Check_To_Headers (T : in out Aunit.Test_Cases.Test_Case'Class) is

      Test_CT   : Content_Type :=
        Content_Type'(MIME_Type     => T_Text,
                      Text_Subtype  => ST_Vnd_D_Sun_D_J2me_D_App_Descriptor,
                      Character_Set => CS_UTF_8,
                      Content_Transfer_Encoding =>
                        CTE_Quoted_Printable);
      Test_Mult   : Content_Type :=
        Content_Type'(MIME_Type           => T_Multipart,
                      Multipart_Subtype   => ST_FORM_DATA,
                      Multipart_Delimiter => US ("(qw!@#$%^*&)"),
                      Content_Transfer_Encoding =>
                        CTE_7Bit);

      Test_HD     : Headers.Lists.List := To_Headers (Test_CT, True);
      Test_HMult  : Headers.Lists.List := To_Headers (Test_Mult, False);
      H_Cursor    : Cursor             := First (Test_HD);
      HM_Cursor   : Cursor             := First (Test_HMult);
      Has_CT      : Boolean            := False;
      Has_CTE     : Boolean            := False;
      Has_Version : Boolean            := False;


   begin -- Check_To_Headers

      while Has_Element (H_Cursor) loop
         if To_Upper (Get_Key (Element (H_Cursor))) = "CONTENT-TYPE" then
            Assert (To_Upper (Get_Value (Element (H_Cursor))) =
                    "TEXT/VND.SUN.J2ME.APP-DESCRIPTOR; CHARSET=UTF-8",
                    "Test_HD: Content Type Line /= Expected");
            Has_CT := True;
         elsif To_Upper (Get_Key (Element (H_Cursor))) =
           "CONTENT-TRANSFER-ENCODING"
         then
            Assert (To_Upper (Get_Value (Element (H_Cursor))) =
                    "QUOTED-PRINTABLE", "Test_HD: CTE Line /= Expected");
            Has_CTE := True;
         elsif  To_Upper (Get_Key (Element (H_Cursor))) =
           "MIME-VERSION"
         then
            Assert (To_Upper (Get_Value (Element (H_Cursor))) =
                    "1.0", "Test_HD: Mime Version /= Expected");
            Has_Version := True;
         else
            Assert (False, "Test_HD: Unknown header in list: " &
                    Get_Value (Element (H_Cursor)));
         end if;
         Next (H_Cursor);
      end loop;

      Assert (Has_CT, "Test_HD: No Content Type");
      Assert (Has_CTE, "Test_HD: No Content Transfer Encoding");
      Assert (Has_Version, "Test_HD: No Version");
      Has_CT      := False;
      Has_CTE     := False;
      Has_Version := False;

      while Has_Element (HM_Cursor) loop
         if To_Upper (Get_Key (Element (HM_Cursor))) = "CONTENT-TYPE" then
            Assert (To_Upper (Get_Value (Element (HM_Cursor))) =
                    "MULTIPART/FORM-DATA; BOUNDARY=""\(QW!\@#$%^*&\)""",
                    "Test_HMult: Content Type Line /= Expected");
            Has_CT := True;
         elsif To_Upper (Get_Key (Element (HM_Cursor))) =
           "CONTENT-TRANSFER-ENCODING"
         then
            Assert (To_Upper (Get_Value (Element (HM_Cursor))) =
                    "7BIT", "Test_HMult: CTE Line /= Expected");
            Has_CTE := True;
         elsif  To_Upper (Get_Key (Element (HM_Cursor))) =
           "MIME-VERSION"
         then
            Has_Version := True;
         else
            Assert (False, "Test_HD: Unknown header in list: " &
                    Get_Value (Element (HM_Cursor)));
         end if;
         Next (HM_Cursor);
      end loop;
      Assert (Has_Version = False, "TEST_HMult: Has Version");

   end Check_To_Headers;


   ----------------------------------------------------------------------------
   procedure Check_From_Headers (T : in out Aunit.Test_Cases.Test_Case'Class) is

      Test_CT   : Content_Type :=
        Content_Type'(MIME_Type     => T_Text,
                      Text_Subtype  => ST_Vnd_D_Sun_D_J2me_D_App_Descriptor,
                      Character_Set => CS_UTF_8,
                      Content_Transfer_Encoding =>
                        CTE_Quoted_Printable);
      Test_Mult   : Content_Type :=
        Content_Type'(MIME_Type           => T_Multipart,
                      Multipart_Subtype   => ST_FORM_DATA,
                      Multipart_Delimiter => US ("(qw!@#$%^*&)"),
                      Content_Transfer_Encoding =>
                        CTE_7Bit);

      Test_HD     : Headers.Lists.List := To_Headers (Test_CT, True);
      Test_PSD    : Content_Type       := From_Headers (Test_HD);
      Test_MD     : Headers.Lists.List := To_Headers (Test_Mult, True);
      Test_PMD    : Content_Type       := From_Headers (Test_MD);

      Scratch_Headers      : Headers.Lists.List;
      Scratch_Content_Type : Content_Type;

   begin -- Check_From_Headers

      Assert (Test_CT = Test_PSD, "Test_CT /= Test_HD");
      Assert (Test_Mult = Test_PMD, "Test_Mult /= Test_PMD");

      Scratch_Headers.Append
        (To_Header ("Content-Type", " text (hi) / tab-separated-values " &
                    "(tabs (are funny));  (:\))charset =(\()jp-ocr-b-add"));
      Scratch_Content_Type := From_Headers (Scratch_Headers);
      Assert (Scratch_Content_Type.MIME_Type = T_Text,
              "JP Text incorrect MIME Type");
      Assert (Scratch_Content_Type.Text_Subtype = ST_Tab_Separated_Values,
              "JP Text incorrect Subtype");
      Assert (Scratch_Content_Type.Character_Set = CS_Jis_C6229_1984_B_Add,
              "JP Text incorrect character set");
      Assert (Scratch_Content_Type.Content_Transfer_Encoding = CTE_7bit,
              "JP Text incorrect CTE (implicit 7bit)");
      Scratch_Headers.Append (To_Header("Content-Transfer-Encoding",
                                        "()8bit((("));
      Scratch_Content_Type := From_Headers (Scratch_Headers);
      Assert (Scratch_Content_Type.Content_Transfer_Encoding = CTE_8bit,
              "JP Text incorrect CTE (explicit 8bit)");

   end Check_From_Headers;

   ----------------------------------------------------------------------------
   procedure Register_Tests (T : in out Test_Case) is

   begin -- Register_Tests

      Register_Routine (T, Check_To_Headers'Access,
                        "Test Serialization of Content Type into Headers");
      Register_Routine (T, Check_From_Headers'Access,
                        "Test Parsing of Headers into Content Type Objects");

   end Register_Tests;

   ----------------------------------------------------------------------------
   function Name (T : Test_Case) return String_Access is

   begin -- Name

      return new String'("Test Content Types");

   end Name;

end Test_Content_Types;
