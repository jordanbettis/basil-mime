--
--                 Test Suite for The Basil Library
--
-- Test Cases for string-related functionality.
--

with Ada.Strings.Unbounded;          use Ada.Strings.Unbounded;
with Ada.Strings.Maps;               use Ada.Strings.Maps;
with Ada.Characters.Latin_1;

with AUnit.Test_Cases;               use AUnit.Test_Cases;
with AUnit.Test_Cases.Registration;  use AUnit.Test_Cases.Registration;
with AUnit.Assertions;               use AUnit.Assertions;

with Ada.Text_IO;                    use Ada.Text_IO;

with Tests_Common;                   use Tests_Common;

with Basil.Typed_Texts;              use Basil.Typed_Texts;
with Basil.Tokens;
with Basil.Multitype_Texts;
with Basil.Strings;                  use Basil.Strings;

use Basil;

with Basil.Strings.Base64;
with Basil.Strings.Quoted_Printable;

package body Test_Strings is

   --------------------------------------------------------------------------
   -- Construct a string with values from 0 to 255 inclusive.
   function All_Values return String is

      Value: Unbounded_String;

   begin -- All_Values

      for I in 0 .. 255 loop
         Value := Value & Character'Val(I);
      end loop;

      return S (Value);

   end All_Values;

   -------------------------------------------------------------------------
   --  Ensure there are no lines longer than 78 characters long;
   function Is_Properly_Wrapped (Encoded_Value : in Encoded_String)
                                return Boolean is

      package L renames Ada.Characters.Latin_1;

      Value         : Unbounded_String := EUS (Encoded_Value);
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

   ----------------------------------------------------------------------------
   --  Ensure there are no lines longer than 78 characters long;
   function No_Long_Words (Encoded_Value : in Encoded_String)
                          return Boolean is

      package L renames Ada.Characters.Latin_1;

      Length : Positive := 1;

   begin -- No_Long_Words

      for I in Encoded_Value'Range loop
         if Length > 78 then
            return False;
         end if;
         if Is_In (Encoded_Value(I), Basil.Tokens.Whitespace) then
            Length := 1;
         else
            Length := Length + 1;
         end if;
      end loop;

      return True;

   end No_Long_Words;

   ----------------------------------------------------------------------------
   --  Test Base64 encoding and decoding operations
   procedure Check_Base64 (T : in out AUnit.Test_Cases.Test_Case'Class) is

      Unencoded    : String         := All_Values;
      Encoded      : Encoded_String := Base64.Encode (Unencoded);
      Encoded_Long : Encoded_String := Base64.Encode (Unencoded, False);
      Decoded_Long : String         := Base64.Decode (Encoded_Long);
      Decoded      : String         := Base64.Decode (Encoded);

   begin -- Check_Base64

      Assert (Unencoded = Decoded,
              "Unencoded Value /= Decoded Value after round trip");
      Assert (Unencoded = Decoded_Long,
              "Unencoded Value /= Decoded_Long Value");
      Assert (Decoded = Decoded_Long,
              "Decoded Value /= Decoded_Long Value");
      Assert (Is_Properly_Wrapped (Encoded),
              "Encoded lines not wrapped correctly");
      Assert (Base64.Encode ("") = "",
              "Handling zero length input failed for encoder");
      Assert (Base64.Decode ("") = "",
              "Handling zero length input failed for decoder");


   end Check_Base64;

   ----------------------------------------------------------------------------
   --  Test proper handling and recovery from bad input to Base64
   --  decoding.
   procedure Check_Base64_Badness
     (T : in out Aunit.Test_Cases.Test_Case'Class) is

      Scratch      : Unbounded_String;
      Error_Caught : Boolean := False;

   begin -- Check_Base64_Badness

      begin
         Scratch := US (Base64.Decode ("12345"));
      exception
         when Invalid_Quoting =>
            Error_Caught := True;
      end;
      Assert (Error_Caught, "Did not catch improper length");
      Error_Caught := False;

      begin
         Scratch := US (Base64.Decode ("AAECAwQF%", Reject_Badness => True));
      exception
         when Invalid_Character =>
            Error_Caught := True;
      end;
      Assert (Error_Caught, "Did not catch illegal character");

   end Check_Base64_Badness;


   -----------------------------------------------------------------------------
   --  Test Quoted_Printable encoding and decoding operations
   procedure Check_Quoted_Printable
     (T : in out AUnit.Test_Cases.Test_Case'Class) is

      package L renames Ada.Characters.Latin_1;

      Unencoded    : String         := All_Values;
      Encoded      : Encoded_String := Quoted_Printable.Encode (Unencoded);
      Encoded_Long : Encoded_String :=
        Quoted_Printable.Encode (Unencoded, Wrap_Long_Lines => False);
      Decoded_Long : String         := Quoted_Printable.Decode (Encoded_Long);
      Decoded      : String         := Quoted_Printable.Decode (Encoded);

      Soft_Break   : Encoded_String := "=" & L.CR & L.LF;
      Fold_Test    : Encoded_String := Quoted_Printable.Encode
        ("Test" & L.CR & L.LF & "test " & L.LF & "test" & L.CR);
      Fold_Valid   : Encoded_String
        := "Test=0D=0A" & Soft_Break & "test =0A" & Soft_Break & "test=0D"
        & Soft_Break;
      Fold_Test_2  : Encoded_String := Quoted_Printable.Encode
        (L.LF & L.LF & L.CR & L.LF & L.CR & L.CR & L.LF);
      Fold_Valid_2 : Encoded_String
        := "=0A" & Soft_Break & "=0A" & Soft_Break & "=0D=0A" & Soft_Break
        & "=0D" & Soft_Break & "=0D=0A" & Soft_Break;

      Additional_Test  : Encoded_String := Quoted_Printable.Encode
        ("Test", Quote_Additional => To_Set ("st"));
      Additional_Valid : Encoded_String := "Te=73=74";

   begin -- Check_Quoted_Printable

      Assert (Unencoded = Decoded,
              "Unencoded Value /= Decoded Value after round trip");
      Assert (Unencoded = Decoded_Long,
              "Unencoded Value /= Decoded_Long Value");
      Assert (Decoded = Decoded_Long,
              "Decoded Value /= Decoded_Long Value");
      Assert (Is_Properly_Wrapped (Encoded),
              "Encoded lines not wrapped correctly");
      Assert (Fold_Test = Fold_Valid,
              "Line Wrapping Test One failed");
      Assert (Fold_Test_2 = Fold_Valid_2,
              "Line_Wrapping Test Two failed");
      Assert (Additional_Test = Additional_Valid,
              "Quoting of additional character set failed");
      Assert (Quoted_Printable.Decode ("=7a") = "z",
              "Handling lowercase Hex number failed");
      Assert (Quoted_Printable.Encode ("") = "",
              "Handling zero length input failed for encoder");
      Assert (Quoted_Printable.Decode ("") = "",
              "Handling zero length input failed for decoder");

   end Check_Quoted_Printable;

   ----------------------------------------------------------------------------
   --  Test proper handling and recovery from bad input to Quoted_Printable.
   procedure Check_Quoted_Printable_Badness
     (T : in out Aunit.Test_Cases.Test_Case'Class) is

      Scratch      : Unbounded_String;
      Error_Caught : Boolean := False;

   begin -- Check_Quoted_Printable_Badness

      begin
         Scratch := US (Quoted_Printable.Decode
                        ("=7a", Reject_Badness => True));
      exception
         when Invalid_Character =>
            Error_Caught := True;
      end;
      Assert (Error_Caught, "Did not catch lowercase Hex number");
      Error_Caught := False;

      begin
         Scratch := US (Quoted_Printable.Decode
                        ("=7-54", Reject_Badness => True));
      exception
         when Invalid_Character =>
            Error_Caught := True;
      end;
      Assert (Error_Caught, "Did not catch improper length Hex number");


   end Check_Quoted_Printable_Badness;

   ----------------------------------------------------------------------------
   --  Test basic quoting/unquoting of RFC 2047 headers
   procedure Check_Header_Quoting
     (T : in out Aunit.Test_Cases.Test_Case'Class) is

      function Construct_Test_List return Multitype_Texts.List is

         Working_List : Multitype_Texts.List;

      begin -- Construct_Test_List

         Multitype_Texts.Append
           (Working_List, To_Typed_Text("Hello ", CS_US_ASCII));
         Multitype_Texts.Append
           (Working_List, To_Typed_Text("This is a test,", CS_ISO_8859_1));
         Multitype_Texts.Append
           (Working_List, To_Typed_Text(" François", CS_UTF_8));
         Multitype_Texts.Append
           (Working_List, To_Typed_Text("<Bayrou>", CS_UTF_8));
         Multitype_Texts.Append
           (Working_List, To_Typed_Text("", CS_ISO_2022_JP));
         Multitype_Texts.Append
           (Working_List, To_Typed_Text("  IBM  ", CS_EBCDIC_US));
         Multitype_Texts.Append
           (Working_List, To_Typed_Text(" ", CS_US_ASCII));

         return Working_List;

      end Construct_Test_List;

      package L renames Ada.Characters.Latin_1;

      Soft_Break       : String     := "=" & L.CR & L.LF;
      Utf8_Text        : Typed_Text := To_Typed_Text
        ("<Laurent> <Fabius> et François Bayrou ont critiqué, samedi, le " &
         "redéploiement partiel de la prime pour l'emploi pour financer " &
         "le RSA. Ils craignent que les salariés modestes soient pénalisés " &
         "pour aider les plus pauvres." & Soft_Break, CS_UTF_8);
      Empty_Text       : Typed_Text := To_Typed_Text ("", CS_ISO_8859_1);
      Non_Ascii_Text   : Typed_Text := To_Typed_Text ("Test Message",
                                                     CS_ISO_2022_JP);
      Non_Ascii_Quoted : Encoded_String := Quote_Header (Non_Ascii_Text);
      Working_List     : Multitype_Texts.List;
      Test_List        : Multitype_Texts.List := Construct_Test_List;

   begin -- Check_Header_Quoting

      Assert (Unquote_Header(Quote_Header (Utf8_Text), Reject_Badness => True)
              = Utf8_Text, "Unencoded Utf8_Text /= Utf8_Text with QP");
      Assert (Unquote_Header(Quote_Header (Utf8_Text, CTE_Base64),
                             Reject_Badness => True)
              = Utf8_Text, "Unencoded Utf8_Text /= Utf8_Text with Base64");
      Assert (Quote_Header (Empty_Text) = "",
              "Quoting empty text failed");
      Assert (String'(Get_Value (Unquote_Header(""))) = "",
              "Unquoting empty string failed");
      Assert (String'(Get_Value (Unquote_Header ("=?us-ascii?Q?Keith_Moore?=")))
              = "Keith Moore", "Did not decode underscore as space");
      Assert (String'(Get_Value (Unquote_Header (Quote_Header (All_Values,
                                                               CS_US_ASCII),
                                                 Reject_Badness => True)))
              = All_Values, "Unencoded All_Values /= All_Values");
      Assert (No_Long_Words (Quote_Header (All_Values, CS_US_ASCII)),
              "Encoded words are longer than maximum allowed");
      Assert (Quote_Header(Test_List) = "Hello This is a =?ISO-8859-1?q?test"
              & "=2C?= =?UTF-8?q?Fran=C3=A7ois=3CBayrou=3E?= =?EBCDIC-US?q?="
              & "20=20IBM=20=20?= ", "Quoted Test_List /= expected value");

   end Check_Header_Quoting;

   ----------------------------------------------------------------------------
   --  Header comment unquoting from RFC 2047 p 12
   procedure Check_Header_Comments
     (T : in out Aunit.Test_Cases.Test_Case'Class) is

   begin -- Check_Header_Comments

      Assert (String'(Get_Value (Unquote_Header("(=?ISO-8859-1?Q?a?=)")))
              = "(a)", "Test failed for (a)");
      Assert (String'(Get_Value (Unquote_Header("(=?ISO-8859-1?Q?a?= b)")))
              = "(a b)", "Test failed for (a b)");
      Assert (String'(Get_Value
                      (Unquote_Header
                       ("(=?ISO-8859-1?Q?a?=   =?ISO-8859-1?Q?b?=)")))
              = "(ab)", "Test failed for (ab)");

   end Check_Header_Comments;

   ----------------------------------------------------------------------------
   --  Test detection of bad input header dequoting
   procedure Check_Header_Badness
     (T : in out Aunit.Test_Cases.Test_Case'Class) is

      Scratch      : Typed_Text;
      Error_Caught : Boolean := False;

   begin -- Check_Header_Badness

      begin
         Scratch := Unquote_Header("=?INVALID?q?lkjsf?=",
                                   Reject_Badness => True);
      exception
         when Invalid_Character_Set =>
            Error_Caught := True;
      end;
      Assert (Error_Caught, "Did not catch invalid character set");
      Error_Caught := False;

      begin
         Scratch := Unquote_Header("=?US-ASCII?t?lkjsf?=",
                                   Reject_Badness => True);
      exception
         when Invalid_Quoting =>
            Error_Caught := True;
      end;
      Assert (Error_Caught, "Did not catch invalid content transfer encoding");
      Error_Caught := False;

      begin
         Scratch := Unquote_Header("=?US-ASCII?b??=",
                                   Reject_Badness => True);
      exception
         when Invalid_Quoting =>
            Error_Caught := True;
      end;
      Assert (Error_Caught, "Did not catch empty enquoted word");
      Error_Caught := False;

      begin
         Scratch := Unquote_Header("=?US-ASCII?b?sfd?=",
                                   Reject_Badness => True);
      exception
         when Invalid_Quoting =>
            Error_Caught := True;
      end;
      Assert (Error_Caught, "Did not catch improper length Base64");
      Error_Caught := False;

      begin
         Scratch := Unquote_Header("=?US-ASCII?q?=6a?=",
                                   Reject_Badness => True);
      exception
         when Invalid_Character =>
            Error_Caught := True;
      end;
      Assert (Error_Caught, "Did not catch improper quoted printable hex");
      Error_Caught := False;

   end Check_Header_Badness;

   ----------------------------------------------------------------------------
   --  The RFC 2045 tools are very simple and just reuse routines that
   --  have already been extensivly tested above.
   procedure Check_RFC2045 (T : in out Aunit.Test_Cases.Test_Case'Class) is

   begin -- Check_RFC2045

      Assert (Unquote_RFC2045 (Quote_RFC2045 (All_Values), CTE_Base64)
              = All_Values, "Unquoted All_Values /= All_Values with Base64");
      Assert (Unquote_RFC2045 (Quote_RFC2045 (All_Values, CTE_Quoted_Printable),
                               CTE_Quoted_Printable) = All_Values,
              "Unquoted All_Values /= All_Values with QP");
      Assert (Is_Properly_Wrapped (Quote_RFC2045 (All_Values)),
              "Encoded value not properly wrapped with Base64");
      Assert (Is_Properly_Wrapped
              (Quote_RFC2045 (All_Values, CTE_Quoted_Printable)),
              "Encoded value not properly wrapped with QP");

   end Check_RFC2045;

   ----------------------------------------------------------------------------
   --  Check Ensure_Message_Newlines and Ensure_Unix_Newlines
   procedure Check_Newline_Normalization
     (T : in out Aunit.Test_Cases.Test_Case'Class) is

      package L renames Ada.Characters.Latin_1;

   begin -- Check_Newline_Normalization

      Assert (Ensure_Message_Newlines ("") = "",
              "Ensure_Message_Newlines empty string test failed");
      Assert (Ensure_Message_Newlines (L.LF & L.LF & L.CR) =
              L.CR & L.LF & L.CR & L.LF & L.CR & L.LF,
              "Ensure_Message_Newlines LF LF CR /= CR LF CR LF CR LF");
      Assert (Ensure_Message_Newlines (L.CR & L.LF & 'A' & L.CR & L.CR) =
              L.CR & L.LF & 'A' & L.CR & L.LF & L.CR & L.LF,
              "Ensure_Message_Newlines CR LF A CR CR /= CR LF A CR LF CR LF");
      Assert (Ensure_Message_Newlines (L.CR & 'A') =  L.CR & L.LF & 'A',
              "Ensure_Message_Newlines CR A /= CR LF A");
      Assert (Ensure_Message_Newlines ("A") = "A",
              "Ensure_Message_Newlines A /= A");

      Assert (Ensure_Unix_Newlines ("") = "",
              "Ensure_Unix_Newlines empty string test failed");
      Assert (Ensure_Unix_Newlines (L.LF & L.LF & L.CR) =
              L.LF & L.LF & L.LF, "Ensure_Unix_Newlines LF LF CR /= LF LF LF");
      Assert (Ensure_Unix_Newlines (L.CR & L.LF & 'A' & L.CR & L.CR) =
              L.LF & 'A' & L.LF & L.LF,
              "Ensure_Unix_Newlines CR LF A CR CR /= LF A LF LF");
      Assert (Ensure_Unix_Newlines (L.CR & 'A') =  L.LF & 'A',
              "Ensure_Unix_Newlines CR A /= LF A");
      Assert (Ensure_Unix_Newlines ("A") = "A",
              "Ensure_Unix_Newlines A /= A");

   end Check_Newline_Normalization;

   ----------------------------------------------------------------------------
   procedure Register_Tests (T : in out Test_Case) is

   begin -- Register_Tests

      Register_Routine (T, Check_Base64'Access,
                        "Test Base64 Encoding/Decoding");
      Register_Routine (T, Check_Base64_Badness'Access,
                        "Test Base64 Bad Input");
      Register_Routine (T, Check_Quoted_Printable'Access,
                        "Test Quoted-Printable Encoding/Decoding");
      Register_Routine (T, Check_Quoted_Printable_Badness'Access,
                        "Test Quoted-Printable Bad Input");
      Register_Routine (T, Check_Header_Quoting'Access,
                        "Test Header Quoting/Unquoting");
      Register_Routine (T, Check_Header_Comments'Access,
                        "Test Header Unquoting with Comments");
      Register_Routine (T, Check_Header_Badness'Access,
                        "Test Header Unquoting Bad Input");
      Register_Routine (T, Check_RFC2045'Access,
                        "Test RFC 2045 Quoting/Unquoting");
      Register_Routine (T, Check_Newline_Normalization'Access,
                        "Test Newline Normalization Functions");

   end Register_Tests;

   ----------------------------------------------------------------------------
   function Name (T : Test_Case) return String_Access is

   begin -- Name

      return new String'("Test Strings");

   end Name;


end Test_Strings;
