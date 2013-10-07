-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--
--                  BASIL MESSAGE HANDLING LIBRARY
--
--                Copyright (C) 2008, Jordan Bettis
--             GNU GPL (General Public License) Notice
--
--  Basil is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 2, or (at your option)
--  any later version.  Basil is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--  See the GNU General Public License for more details.  You should
--  have received a copy of the GNU General Public License distributed
--  with Basil; see file COPYING.  If not, write to the Free Software
--  Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
--  02110-1301, USA.
--
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;               use Ada.Strings.Fixed;
with Ada.Strings.Maps;                use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants;      use Ada.Strings.Maps.Constants;
with Ada.Characters.Handling;         use Ada.Characters.Handling;
with Ada.Characters.Latin_1;
use Ada.Strings;

with Basil.Strings.Quoted_Printable;  use Basil.Strings.Quoted_Printable;
with Basil.Strings.Base64;            use Basil.Strings.Base64;
with Basil.Utils;                     use Basil.Utils;
with Basil.Tokens;                    use Basil.Tokens;

with Basil.Typed_Texts;
with Basil.Multitype_Texts;

with Basil.Character_Set_Translation;
use Basil.Character_Set_Translation;

package body Basil.Strings is

   ----------------------------------------------------------------------------
   --  This function fetches a word, as defined by RFC 2047 from the
   --  input string starting at the index. It then updates the index
   --  to point to the first character after the last character of the
   --  word. For the purpose of this function any leading or trailing
   --  whitespace is included in the word.
   procedure Get_RFC2047_Word (Input : in     String;
                               Index : in out Natural;
                               Word  :    out Unbounded_String);

   --------------------------------------------------------------------------
   --  This procedure enquotes the provided word with RFC 2047
   --  encapsulation in the specified encoding with a lable for the
   --  given character set inserted into encapsulation preamble. If
   --  the word is too long it will split it using Split_Encoded_Word.
   --
   --  If Encode_Whitespace is True, the function will quote
   --  any leading or trailing whitespace.
   procedure Quote_Word (Word              : in out Unbounded_String;
                         Character_Set     : in     Character_Set_ID;
                         Encoding          : in     Supported_Encoding;
                         Encode_Whitespace : in     Boolean);

   ----------------------------------------------------------------------------
   --  In order to allow wrapping of headers containing long encoded
   --  words (and to comply with RFC 2047) we must break them into <
   --  76 character segments. This function is configured to break at
   --  no more than 60 characters to allow some breathing room.
   function Split_Encoded_Word (Word          : in String;
                                Character_Set : in Character_Set_ID;
                                Encoding      : in Supported_Encoding)
                               return String;

   ----------------------------------------------------------------------------
   --  This function performs all the steps necessary to enquote a
   --  string whose character set is compatable with US-ASCII, by
   --  encapsulating any word containing illegal characters, merging
   --  contigious encapsulated words, and splitting oversized
   --  encapsulated words.
   function Quote_ASCII_Header (Unquoted_Value       : in String;
                                Character_Set        : in Character_Set_ID;
                                Requested_Encoding   : in Supported_Encoding)
                               return Encoded_String;

   ----------------------------------------------------------------------------
   --  These functions take a word and perform the named operation on the
   --  word, returning the result. The definition of "Whitespace" is the
   --  whitespace character set.
   function Trim_Whitespace         (Word : in String) return String;
   function Get_Leading_Whitespace  (Word : in String) return String;
   function Get_Trailing_Whitespace (Word : in String) return String;

   ----------------------------------------------------------------------------
   --  This function returns true if we consider the particular
   --  character set to be either congruent with or a pure superset of
   --  US-ASCII.
   function Is_Compatable_With_ASCII (Character_Set : in Character_Set_ID)
                                     return Boolean;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function To_Encoded_String (Input : String) return Encoded_String is

      Output : Encoded_String (Input'Range);

   begin -- To_Encoded_String

      for I in Input'Range loop
         Output (I) := Encoded_Character (Input (I));
      end loop;

      return Output;

   end To_Encoded_String;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function To_Encoded_String (Input : Unbounded_String)
                              return Encoded_String is

      Output : Encoded_String :=
        To_Encoded_String (To_String (Input));

   begin -- To_Encoded_String

      return Output;

   end To_Encoded_String;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function To_String (Input : Encoded_String) return String is

      Output : String (Input'Range);

   begin -- To_String

      for I in Input'Range loop
         Output(I) := Character (Input (I));
      end loop;

      return Output;

   end To_String;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function To_Unbounded_String (Input : Encoded_String)
                                return Unbounded_String is

      Output : Unbounded_String :=
        To_Unbounded_String (To_String (Input));

   begin -- To_Unbounded_String

      return Output;

   end To_Unbounded_String;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Quote_Header (Unquoted_Value       : in String;
                          Character_Set        : in Character_Set_ID;
                          Requested_Encoding   : in Supported_Encoding
                          := CTE_Quoted_Printable)
                         return Encoded_String is

      Non_ASCII_Word : Unbounded_String;

   begin -- Quote_Header

      if Is_Compatable_With_ASCII (Character_Set) then

         return Quote_ASCII_Header (Unquoted_Value,
                                    Character_Set,
                                    Requested_Encoding);

      else

         Non_ASCII_Word := US (Unquoted_Value);

         Quote_Word (Word              => Non_ASCII_Word,
                     Character_Set     => Character_Set,
                     Encoding          => Requested_Encoding,
                     Encode_Whitespace => True);

         return ES (Non_ASCII_Word);

      end if;

   end Quote_Header;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Quote_Header (Unquoted_Value       : in Typed_Texts.Typed_Text;
                          Requested_Encoding   : in Supported_Encoding
                          := CTE_Quoted_Printable)
                         return Encoded_String is

   begin -- Quote_Header

      return Quote_Header (Typed_Texts.Get_Value         (Unquoted_Value),
                           Typed_Texts.Get_Character_Set (Unquoted_Value),
                           Requested_Encoding);

   end Quote_Header;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Quote_Header (Unquoted_Value     : in Multitype_Texts.List;
                          Requested_Encoding : in Supported_Encoding
                          := CTE_Quoted_Printable)
                         return Encoded_String is

      use Basil.Multitype_Texts;
      use Basil.Typed_Texts;

      -------------------------------------------------------------------------
      --  We want all contigious text objects with the same character
      --  set to be combined into unified segments.
      procedure Get_Next_Segment
        (Text_Cursor           : in out Multitype_Texts.Cursor;
         Working_Segment       :    out Unbounded_String;
         Working_Character_Set :    out Character_Set_ID);

      procedure Get_Next_Segment
        (Text_Cursor           : in out Multitype_Texts.Cursor;
         Working_Segment       :    out Unbounded_String;
         Working_Character_Set :    out Character_Set_ID) is

      begin -- Get_Next_Segment

         Working_Character_Set := Get_Character_Set (Element (Text_Cursor));
         Working_Segment       := US ("");

         while Text_Cursor /= No_Element and then
           Get_Character_Set (Element (Text_Cursor)) = Working_Character_Set
         loop

            Append (Working_Segment, US (Get_Value (Element (Text_Cursor))));

            Next (Text_Cursor);

         end loop;

      end Get_Next_Segment;

      -------------------------------------------------------------------------
      procedure Combine_Quoted_Values
        (Quoted_Value    : in out Unbounded_String;
         Working_Segment : in     Unbounded_String);

      procedure Combine_Quoted_Values
        (Quoted_Value    : in out Unbounded_String;
         Working_Segment : in     Unbounded_String) is

      begin -- Combine_Quoted_Values

         if (Length (Quoted_Value) > 0 and Length (Working_Segment) > 0)
           and then
           (not Is_In (Element (Quoted_Value, Length (Quoted_Value)),
                       Whitespace)
            and not Is_In (Element (Working_Segment, 1), Whitespace))
         then
            Append (Quoted_Value, " ");
         end if;

         Append (Quoted_Value, Working_Segment);

      end Combine_Quoted_Values;

      -------------------------------------------------------------------------
      Working_Segment       : Unbounded_String;
      Working_Character_Set : Character_Set_ID;
      Quoted_Value          : Unbounded_String;

      Text_Cursor           : Multitype_Texts.Cursor
        := Multitype_Texts.First (Unquoted_Value);

   begin -- Quote_Header

      while Text_Cursor /= No_Element loop

         Get_Next_Segment (Text_Cursor           => Text_Cursor,
                           Working_Segment       => Working_Segment,
                           Working_Character_Set => Working_Character_Set);

         if Is_Compatable_With_ASCII (Working_Character_Set) then

            Combine_Quoted_Values
              (Quoted_Value, EUS (Quote_ASCII_Header (S (Working_Segment),
                                                      Working_Character_Set,
                                                      Requested_Encoding)));

         else

            Quote_Word (Word              => Working_Segment,
                        Character_Set     => Working_Character_Set,
                        Encoding          => Requested_Encoding,
                        Encode_Whitespace => True);

            Combine_Quoted_Values (Quoted_Value, Working_Segment);

         end if;

      end loop;

      return ES (Quoted_Value);

   end Quote_Header;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Unquote_Header (Quoted_Value   : in Encoded_String;
                            Reject_Badness : in Boolean := False)
                           return Multitype_Texts.List is

      -------------------------------------------------------------------------
      function Is_Quoted (Word: in Unbounded_String) return Boolean;

      function Is_Quoted (Word: in Unbounded_String) return Boolean is

         Word_Value : String := Trim_Whitespace (S (Word));

      begin -- Is_Quoted

         --  The shortest possible unquoted word, with a one-character
         --  encoding name and no payload would be "=?x?x??="
         if Length (Word) < 8 then
            return False;
         end if;

         if Word_Value (Word_Value'First .. Word_Value'First + 1) = "=?" and
           Word_Value (Word_Value'Last - 1 .. Word_Value'Last) = "?="
         then
            return True;
         else
            return False;
         end if;

      end Is_Quoted;

      -------------------------------------------------------------------------
      --  If reject_badness is false, and there is a problem, then
      --  this procedure returns without modifying the word. If it is
      --  true then this procedure raises an exception.
      procedure Unquote_Word (Word           : in out Unbounded_String;
                              Character_Set  :    out Character_Set_ID;
                              Successful     :    out Boolean;
                              Reject_Badness : in     Boolean);

      procedure Unquote_Word (Word           : in out Unbounded_String;
                              Character_Set  :    out Character_Set_ID;
                              Successful     :    out Boolean;
                              Reject_Badness : in     Boolean) is

         Character_Set_Label : Unbounded_String;
         Encoding_Label      : String (1 .. 1);
         Encoded_Value       : Unbounded_String;
         Working_Word        : Unbounded_String  := Word;

      begin -- Unquote_Word

         if Head (Working_Word, 2) = "=?" then
            Tail (Working_Word, Length (Working_Word) - 2);
         elsif Reject_Badness then
            raise Invalid_Quoting;
         else
            Character_Set := CS_US_ASCII;
            Successful    := False;
            return;
         end if;

         Get_Token (Source => Working_Word,
                    Token  => Character_Set_Label,
                    Set    => RFC2047_Token);

         Discard_Token (Source => Working_Word,
                        Set    => not RFC2047_Token);

         Encoding_Label := S (Head (Working_Word, Encoding_Label'Length));
         Tail (Working_Word, Length (Working_Word) - Encoding_Label'Length);

         Discard_Token (Source => Working_Word,
                        Set    => not RFC2047_Encoded_Text);

         Get_Token (Source => Working_Word,
                    Token  => Encoded_Value,
                    Set    => RFC2047_Encoded_Text);

         -- The previous token should be the remaining string except "?="
         if S (Working_Word) /= "?=" and Reject_Badness then
            raise Invalid_Quoting;
         elsif S (Working_Word) /= "?=" and not Reject_Badness then
            Character_Set := CS_US_ASCII;
            Successful    := False;
            return;
         end if;

         -- Cs_Unknown means it's not a valid MIME charset id
         Character_Set := Get_Identifier (S (Character_Set_Label));
         if Character_Set = CS_Unknown and Reject_Badness then
            raise Invalid_Character_Set;
         elsif Character_Set = CS_Unknown and not Reject_Badness then
            Character_Set := CS_US_ASCII;
            Successful    := False;
            return;
         end if;

         -- The only legal values here are 'Q', 'q', 'B', and 'b'.
         if Encoding_Label = "q" or Encoding_Label = "Q" then
            -- RFC 2047 permits implementations to use _ instead of =20
            -- to represent space (and considers _ to be an especial)
            Replace_All (Encoded_Value,
                         Target      => "_",
                         Replacement => "=20");
            Working_Word := US (Quoted_Printable.Decode (ES( S(Encoded_Value)),
                                                         Reject_Badness));
         elsif Encoding_Label = "b" or Encoding_Label = "B" then
            Working_Word := US (Base64.Decode (ES (S (Encoded_Value))));
         elsif Reject_Badness then
            raise Invalid_Quoting;
         else
            Character_Set := CS_US_ASCII;
            Successful    := False;
            return;
         end if;

         Successful := True;
         Word       := Working_Word;

      end Unquote_Word;

      -------------------------------------------------------------------------
      procedure Append_To_Unquoted_Text
        (Unquoted_Text : in out Multitype_Texts.List;
         Value         : in     Unbounded_String;
         Character_Set : in     Character_Set_ID);

      procedure Append_To_Unquoted_Text
        (Unquoted_Text : in out Multitype_Texts.List;
         Value         : in     Unbounded_String;
         Character_Set : in     Character_Set_ID) is

         use Basil.Multitype_Texts;
         use Basil.Typed_Texts;

         Previous_Text : Typed_Texts.Typed_Text;

      begin -- Append_To_Unquoted_Text

         if Length (Value) = 0 then
            return;
         end if;

         if Unquoted_Text /= Empty_List and then
           Get_Character_Set (Last_Element (Unquoted_Text)) = Character_Set
         then

            Previous_Text := Last_Element (Unquoted_Text);
            Delete_Last (Unquoted_Text);

            Append (Container => Unquoted_Text,
                    New_Item  => To_Typed_Text
                      (US (Get_Value (Previous_Text)) & Value,
                       Character_Set));

         else

            Append (Container => Unquoted_Text,
                    New_Item  => To_Typed_Text (Value, Character_Set));

         end if;

      end Append_To_Unquoted_Text;

      -------------------------------------------------------------------------
      --  RFC 2047 allows header enquoters to add space between
      --  encoded words that were not in the original text. This is
      --  because words must not be longer than 75 characters but
      --  encoded words must be seperated by whitespace.
      --
      --  We must detect such whitespace and remove it as part of the
      --  unquoting process.
      procedure Discard_Previous_Trailing_Whitespace
        (Unquoted_Text        : in out Multitype_Texts.List;
         Length_Of_Whitespace : in     Natural);

      procedure Discard_Previous_Trailing_Whitespace
        (Unquoted_Text        : in out Multitype_Texts.List;
         Length_Of_Whitespace : in     Natural) is

         use Basil.Multitype_Texts;
         use Basil.Typed_Texts;

         Previous_Value : Unbounded_String;

      begin -- Discard_Previous_Trailing_Whitespace

         if Length_Of_Whitespace = 0 then
            return;
         end if;

         if Unquoted_Text /= Empty_List and then
           Get_Character_Set (Last_Element (Unquoted_Text)) = CS_US_ASCII
         then

            Previous_Value := Get_Value (Last_Element (Unquoted_Text));
            Delete_Last (Unquoted_Text);

            if Length (Previous_Value) > Length_Of_Whitespace then

               Head (Previous_Value,
                     Length (Previous_Value) - Length_Of_Whitespace);

               Append (Container => Unquoted_Text,
                       New_Item  => To_Typed_Text (Previous_Value,
                                                   CS_US_ASCII));

            end if;

         end if;

      end Discard_Previous_Trailing_Whitespace;

      -------------------------------------------------------------------------
      use Basil.Multitype_Texts;
      use Basil.Typed_Texts;

      Index                    : Natural            := 1;

      Word                     : Unbounded_String;
      Leading_Whitespace       : Unbounded_String;
      Trailing_Whitespace      : Unbounded_String;
      Character_Set            : Character_Set_ID   := CS_US_ASCII;

      Word_Is_Quoted           : Boolean            := False;
      Previous_Word_Is_Quoted  : Boolean            := False;
      Successful               : Boolean            := False;

      Unquoted_Text            : Multitype_Texts.List := Empty_List;

      Len_Trailing_Whitespace      : Natural := 0;
      Len_Prev_Trailing_Whitespace : Natural := 0;

   begin -- Unquote_Header

      Get_RFC2047_Word (S (Quoted_Value), Index, Word);
      Leading_Whitespace  := US (Get_Leading_Whitespace  (S (Word)));
      Trailing_Whitespace := US (Get_Trailing_Whitespace (S (Word)));
      Trim (Word, Right => Whitespace, Left => Whitespace);

      Len_Trailing_Whitespace := Length (Trailing_Whitespace);

      while Word /= US ("") loop

         if Is_Quoted (Word) then
            Unquote_Word (Word           => Word,
                          Character_Set  => Character_Set,
                          Successful     => Successful,
                          Reject_Badness => Reject_Badness);
            if not Successful then
               Word_Is_Quoted := False;
            else
               Word_Is_Quoted := True;
            end if;
         else
            Word_Is_Quoted := False;
         end if;

         if Word_Is_Quoted and Previous_Word_Is_Quoted then

            Discard_Previous_Trailing_Whitespace
              (Unquoted_Text        => Unquoted_Text,
               Length_Of_Whitespace => Len_Prev_Trailing_Whitespace);

         else

            Append_To_Unquoted_Text (Unquoted_Text => Unquoted_Text,
                                     Value         => Leading_Whitespace,
                                     Character_Set => CS_US_ASCII);

         end if;

         Append_To_Unquoted_Text (Unquoted_Text => Unquoted_Text,
                                  Value         => Word,
                                  Character_Set => Character_Set);

         Append_To_Unquoted_Text (Unquoted_Text => Unquoted_Text,
                                  Value         => Trailing_Whitespace,
                                  Character_Set => CS_US_ASCII);


         Get_RFC2047_Word (S (Quoted_Value), Index, Word);
         Leading_Whitespace  := US (Get_Leading_Whitespace  (S (Word)));
         Trailing_Whitespace := US (Get_Trailing_Whitespace (S (Word)));
         Trim (Word, Right => Whitespace, Left => Whitespace);

         Len_Prev_Trailing_Whitespace := Len_Trailing_Whitespace;
         Len_Trailing_Whitespace := Length (Trailing_Whitespace);

         if Word_Is_Quoted then
            Previous_Word_Is_Quoted := True;
         else
            Previous_Word_Is_Quoted := False;
         end if;

      end loop;

      return Unquoted_Text;

   end Unquote_Header;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Unquote_Header (Quoted_Value   : in Encoded_String;
                            Reject_Badness : in Boolean := False)
                           return Typed_Texts.Typed_Text is

      use Basil.Multitype_Texts;
      use Basil.Typed_Texts;

      Decoded_Text : Multitype_Texts.List   := Unquote_Header (Quoted_Value,
                                                               Reject_Badness);
      Cursor       : Multitype_Texts.Cursor := First (Decoded_Text);

      Working_Text   : Typed_Text;

      Unquoted_Value : Unbounded_String;
      Character_Set  : Character_Set_ID     := CS_US_ASCII;

   begin -- Unquote_header;

      while Cursor /= No_Element loop

         Working_Text := Element (Cursor);

         if Get_Character_Set (Working_Text) /= CS_US_ASCII then

            if Character_Set = CS_US_ASCII then
               Character_Set := Get_Character_Set (Working_Text);
            elsif Character_Set /= Get_Character_Set (Working_Text) then
               raise Invalid_Character_Set;
            end if;

         end if;

         Append (Unquoted_Value, US (Get_Value (Working_Text)));

         Next (Cursor);

      end loop;

      return To_Typed_Text (Unquoted_Value, Character_Set);

   end Unquote_Header;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   --  This function takes a string containing text or data and quotes
   --  it in either Quoted-Printable or Base64 (it chooses Base64 if
   --  none is specified).
   function Quote_RFC2045 (Unquoted_String : in String;
                           Quoting_Type    : in Supported_Encoding :=
                             CTE_Base64)
                          return Encoded_String is

      Quoted_String : Unbounded_String;

   begin -- Quote_RFC2045;

      if Quoting_Type = CTE_Quoted_Printable then
         Quoted_String := US (S (Quoted_Printable.Encode (Unquoted_String)));
      else
         Quoted_String := US (S (Base64.Encode (Unquoted_String)));
      end if;

      return ES (Quoted_String);

   end Quote_RFC2045;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Unquote_RFC2045 (Quoted_String  : in Encoded_String;
                             Quoting_Type   : in Supported_Encoding;
                             Reject_Badness : in Boolean := False)
                            return String is

      Unquoted_String : Unbounded_String := US ("");

   begin -- Unquote_RFC2045

      if Quoting_Type = CTE_Quoted_Printable then
         Unquoted_String := US (Quoted_Printable.Decode (Quoted_String,
                                                         Reject_Badness));
      else
         Unquoted_String := US (Base64.Decode (Quoted_String,
                                               Reject_Badness));
      end if;

      return S (Unquoted_String);

   end Unquote_RFC2045;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Ensure_Given_Newlines (Value        : in String;
                                   Newline_Type : in String)
                                  return String;

   function Ensure_Given_Newlines (Value        : in String;
                                   Newline_Type : in String)
                                  return String is

      package L renames Ada.Characters.Latin_1;

      Newlines    : Character_Set    := To_Set (L.CR & L.LF);
      Clean_Value : Unbounded_String;
      I           : Natural          := Index (Source => Value,
                                               Set    => Newlines,
                                               From   => Value'First);
      Last_I      : Natural          := 1;

   begin -- Ensure_Given_Newlines

      while I /= 0 loop
         Append (Clean_Value, US (Value(Last_I .. (I - 1))) & Newline_Type);

         if (Value(I) = L.CR and I < Value'Length) and then
           Value(I + 1) = L.LF
         then
            I := I + 2;
         else
            I := I + 1;
         end if;
         if I > Value'Length then
            I      := 0;
            Last_I := 0;
         else
            Last_I := I;
            I := Index (Source => Value,
                        Set    => Newlines,
                        From   => I);
         end if;

      end loop;

      if Last_I /= 0 then
         Append (Clean_Value, US (Value(Last_I .. Value'Last)));
      end if;

      return S (Clean_Value);

   end Ensure_Given_Newlines;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Ensure_Message_Newlines (Value : in String)
                                    return String is

      package L renames Ada.Characters.Latin_1;

   begin -- Ensure_Message_Newlines

      return Ensure_Given_Newlines (Value, L.CR & L.LF);

   end Ensure_Message_Newlines;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Ensure_Unix_Newlines (Value : in String)
                                 return String is

      package L renames Ada.Characters.Latin_1;

   begin -- Ensure_Unix_Newlines

      return Ensure_Given_Newlines (Value, 1 * L.LF);

   end Ensure_Unix_Newlines;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   --  An RFC 2047 word is any atomic text seperated from other tokens
   --  by whitespace. There are also situations where the encoded
   --  words might be contigious with RFC 2822 comment delimiters. For
   --  instance: (=?US-ASCII?q?Comment?=).
   --
   --  To deal with this we treat any RFC 2822 comment delimiters as
   --  seperate words.
   procedure Get_RFC2047_Word (Input  : in     String;
                               Index  : in out Natural;
                               Word   :    out Unbounded_String) is

      -------------------------------------------------------------------------
      End_Leading_Whitespace  : Natural := 0;
      End_Word                : Natural := 0;
      End_Trailing_Whitespace : Natural := 0;
      Improper_Terminus       : Natural := 0;

      Leading_Whitespace  : Unbounded_String;
      Clean_Word          : Unbounded_String;
      Trailing_Whitespace : Unbounded_String;

   begin -- Get_RFC2047_Word

      End_Leading_Whitespace := Get_Token_End (Source => Input,
                                               First  => Index,
                                               Set    => Whitespace);

      if End_Leading_Whitespace /= 0 then
         Leading_Whitespace := US (Input(Index .. End_Leading_Whitespace));
         Index := End_Leading_Whitespace + 1;
      end if;

      --  All specials are a single character.
      if Index <= Input'Last and then
        (Is_In (Input(Index), To_Set("()")))
      then
         End_Word := Index;
      else
         End_Word := Get_Token_End (Source => Input,
                                    First  => Index,
                                    Set    => not (Whitespace
                                                   or To_Set("()")));
      end if;

      if End_Word /= 0 then

         Clean_Word := US (Input(Index .. End_Word));
         Index := End_Word + 1;

      end if;

      End_Trailing_Whitespace := Get_Token_End (Source => Input,
                                                First  => Index,
                                                Set    => Whitespace);

      if End_Trailing_Whitespace /= 0 then
         Trailing_Whitespace := US (Input(Index .. End_Trailing_Whitespace));
         Index := End_Trailing_Whitespace + 1;
      end if;

      Word := Leading_Whitespace & Clean_Word & Trailing_Whitespace;

   end Get_RFC2047_Word;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Quote_Word (Word              : in out Unbounded_String;
                         Character_Set     : in     Character_Set_ID;
                         Encoding          : in     Supported_Encoding;
                         Encode_Whitespace : in     Boolean) is

      Leading_Whitespace  : String := Get_Leading_Whitespace   (S (Word));
      Trailing_Whitespace : String := Get_Trailing_Whitespace  (S (Word));

      Trimmed_Word        : aliased String := Trim_Whitespace  (S (Word));
      Untrimmed_Word      : aliased String := S (Word);

      Working_Word        : access String;

      Quoted_Word         : Unbounded_String;

   begin -- Quote_Word

      if Encode_Whitespace then
         Working_Word := Untrimmed_Word'Access;
      else
         Working_Word := Trimmed_Word'Access;
      end if;

      if Working_Word.all = "" then
         Word := US ("");
         return;
      end if;

      Quoted_Word := US ("=?" & Get_Label (Character_Set));

      if Encoding = CTE_Quoted_Printable then
         Append (Quoted_Word,
                 US ("?q?" & S (Quoted_Printable.Encode
                                (Working_Word.all,
                                 RFC2047_Especials or Whitespace,
                                 Wrap_Long_Lines => False)) & "?="));
      else
         Append (Quoted_Word,
                 US ("?b?" & S (Base64.Encode (Working_Word.all,
                                               Wrap_Long_Lines => False))
                     & "?="));
      end if;

      if Encode_Whitespace then
         Word := US (Split_Encoded_Word (S (Quoted_Word),
                                         Character_Set,
                                         Encoding));
      else
         Word := US (Split_Encoded_Word (Leading_Whitespace & S (Quoted_Word)
                                         & Trailing_Whitespace,
                                         Character_Set,
                                         Encoding));
      end if;

   end Quote_Word;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function  Split_Encoded_Word (Word          : in String;
                                 Character_Set : in Character_Set_ID;
                                 Encoding      : in Supported_Encoding)
                                return String is

      package L renames Ada.Characters.Latin_1;

      Preamble            : Unbounded_String;
      Remaining           : Unbounded_String;
      Split_Words         : Unbounded_String;
      After_Matter_Length : Natural;
      Index               : Natural           := 1;
      Leading_Whitespace  : String := Get_Leading_Whitespace (Word);
      Trailing_Whitespace : Unbounded_String;
      Slice_Length        : Positive;
      Max_Word_Size       : constant Positive := 60;

   begin -- Split_Encoded_Word

      Get_RFC2047_Word (Input => Word,
                        Index => Index,
                        Word  => Remaining);

      After_Matter_Length := Word'Length - Length (Remaining);
      Trailing_Whitespace := US (Get_Trailing_Whitespace (S (Remaining)));

      if Encoding = CTE_Quoted_Printable then
         Preamble := US ("=?" & Get_Label (Character_Set) & "?q?");
      else
         Preamble := US ("=?" & Get_Label (Character_Set) & "?b?");
      end if;

      Trim (Remaining, Right => Whitespace, Left => Whitespace);
      Remaining :=
        US (Slice (Remaining, Length (Preamble) + 1, Length (Remaining) - 2));

      Slice_Length := Max_Word_Size - (Length (Preamble) + 2);

      if Encoding = CTE_Base64 then
         Slice_Length := Slice_Length - (Slice_Length mod 4);
      end if;

      while Length (Remaining) > Slice_Length loop

         if Encoding = CTE_Base64 or
           (Element (Remaining, Slice_Length) /= '=' and
            Element (Remaining, Slice_Length - 1) /= '=')
         then

            Append (Split_Words,
                    Preamble & S (Head (Remaining, Slice_Length)) & "?= ");
            Tail (Remaining, Length (Remaining) - Slice_Length);

         elsif Element (Remaining, Slice_Length) = '=' then

            Append (Split_Words,
                    Preamble & S (Head (Remaining, Slice_Length - 1)) & "?= ");
            Tail (Remaining, Length (Remaining) - (Slice_Length - 1));

         elsif Element (Remaining, Slice_Length - 1) = '=' then

            Append (Split_Words,
                    Preamble & S (Head (Remaining, Slice_Length - 2)) & "?= ");
            Tail (Remaining, Length (Remaining) - (Slice_Length - 2));

         end if;

      end loop;

      return Leading_Whitespace & S (Split_Words) & S (Preamble)
        & S (Remaining) & "?=" & S (Trailing_Whitespace)
        & S (Tail (US (Word), After_Matter_Length));

   end Split_Encoded_Word;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Quote_ASCII_Header (Unquoted_Value       : in String;
                                Character_Set        : in Character_Set_ID;
                                Requested_Encoding   : in Supported_Encoding)
                               return Encoded_String is

      -------------------------------------------------------------------------
      function Needs_Encapsulation (Word : in String) return Boolean;

      function Needs_Encapsulation (Word : in String) return Boolean is

         Trimmed_Word             : String  := Trim_Whitespace (Word);
         Word_Needs_Encapsulation : Boolean := False;

      begin -- Needs_Encapsulation

         for I in Trimmed_Word'Range loop

            if Is_In
              (Trimmed_Word(I), Requires_Quoting or RFC2047_Especials)
            then
               Word_Needs_Encapsulation := True;
               exit; -- We only need to find one.
            end if;

         end loop;

         return Word_Needs_Encapsulation;

      end Needs_Encapsulation;

      -------------------------------------------------------------------------
      --  Two consecutive encoded words should be merged or a decoder will
      --  ignore the whitespace between them.
      procedure Merge_Quoted_Words (Left          : in out Unbounded_String;
                                    Right         : in     Unbounded_String;
                                    Encoding      : in     Supported_Encoding;
                                    Character_Set : in     Character_Set_ID);

      procedure Merge_Quoted_Words (Left          : in out Unbounded_String;
                                    Right         : in     Unbounded_String;
                                    Encoding      : in     Supported_Encoding;
                                    Character_Set : in     Character_Set_ID) is

         Joining_Whitespace  : String :=
           Get_Trailing_Whitespace (S (Left)) &
           Get_Leading_Whitespace  (S (Right));
         Right_Whitespace    : String := Get_Trailing_Whitespace (S (Right));

         Encoded_Whitespace  : Unbounded_String;
         Left_Word           : Unbounded_String;
         Right_Word          : Unbounded_String := Right;
         Last_Base64_Quantum : Unbounded_String;
         Left_Start          : Natural;
         Preamble_Length     : Positive;

      begin -- Merge_Quoted_Words

         Trim (Source => Left, Right => Whitespace, Left => Null_Set);
         Trim (Source => Right_Word, Right => Whitespace, Left => Whitespace);

         Left_Start := Index (Left, Whitespace, Inside, Backward);
         Left_Word := Tail (Left, Length(Left) - Left_Start);
         Head (Left, Left_Start);

         -- cut off the ?= on the end of Left_Word
         Head (Left_Word, Length (Left_Word) - 2);

         -- cut the preamble from the beginning of Right Word
         Preamble_Length :=
           String("=?" & Get_Label (Character_Set) & "?x?")'Length;
         Tail (Right_Word, Length(Right_Word) - Preamble_Length);

         if Encoding = CTE_Quoted_Printable then
            Encoded_Whitespace := EUS (Quoted_Printable.Encode
                                       (Joining_Whitespace,
                                        Whitespace,
                                        Wrap_Long_Lines => False));

            Right_Word := Encoded_Whitespace & Right_Word;

         elsif Encoding = CTE_Base64 and
           (Tail (Left_Word, Length (Left_Word) - 1) = "=" or
            Joining_Whitespace'Length /= 0)
         then

            --  If the end of a base64 left word contains padding or
            --  there is joining whitespace we have to reencode the
            --  last octet of the left word, the whitespace, and the
            --  right word together to ensure no intermedial padding.

            Right_Word := Head (Right_Word, Length (Right_Word) - 2);
            Right_Word := US (Base64.Decode (ES (Right_Word),
                                             Reject_Badness => True));

            Last_Base64_Quantum
              := US (Base64.Decode (ES (Tail (Left_Word, 4))));
            Head (Left_Word, Length (Left_Word) - 4);

            Right_Word := US (S (Base64.Encode
                                 (S (Last_Base64_Quantum
                                     & Joining_Whitespace
                                     & Right_Word),
                                  Wrap_Long_Lines => False)) & "?=");

         end if;

         Append (Left, Split_Encoded_Word (S (Left_Word & Right_Word),
                                           Character_Set,
                                           Encoding) & Right_Whitespace);

      end Merge_Quoted_Words;

      -------------------------------------------------------------------------
      Quoted_Value          : Unbounded_String;
      Word                  : Unbounded_String;
      Index                 : Natural             := 1;
      Previous_Was_Quoted   : Boolean             := False;

   begin -- Quote_ASCII_Header

      Get_RFC2047_Word (Unquoted_Value, Index, Word);

      while Word /= US ("") loop

         if Needs_Encapsulation (S (Word)) then

            Quote_Word (Word              => Word,
                        Character_Set     => Character_Set,
                        Encoding          => Requested_Encoding,
                        Encode_Whitespace => False);

            if Previous_Was_Quoted then

               Merge_Quoted_Words (Left          => Quoted_Value,
                                   Right         => Word,
                                   Encoding      => Requested_Encoding,
                                   Character_Set => Character_Set);
            else

               Append (Quoted_Value, Word);

            end if;

            Previous_Was_Quoted := True;

         else

            Previous_Was_Quoted := False;
            Append (Quoted_Value, Word);

         end if;

         Get_RFC2047_Word (Unquoted_Value, Index, Word);

      end loop;

      return ES (S (Quoted_Value));

   end Quote_ASCII_Header;


   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Trim_Whitespace (Word : in String) return String is

   begin -- Trim_Whitespace

      return S (Trim (Source => US (Word),
                      Right  => Whitespace,
                      Left   => Whitespace));

   end Trim_Whitespace;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_Leading_Whitespace  (Word : in String) return String is

      Leading_Whitespace : Unbounded_String;

   begin -- Get_Leading_Whitespace

      for I in Word'Range loop
         if Is_In (Word(I), Whitespace) then
            Append (Leading_Whitespace, Word(I));
         else
            exit;
         end if;
      end loop;

      return S (Leading_Whitespace);

   end Get_Leading_Whitespace;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_Trailing_Whitespace (Word : in String) return String is

      Trailing_Whitespace : Unbounded_String;

   begin -- Get_Trailing_Whitespace

      for I in reverse Word'Range loop
         if Is_In (Word(I), Whitespace) then
            -- Stick the characters on the front since we're going backwards.
            Trailing_Whitespace := Word(I) & Trailing_Whitespace;
         else
            exit;
         end if;
      end loop;

      return S (Trailing_Whitespace);

   end Get_Trailing_Whitespace;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   --  I'm going to be conservative here because I don't have a
   --  comprehensive list, but we can add more sets to this list as
   --  information becomes avaliable.
   function Is_Compatable_With_ASCII (Character_Set : in Character_Set_ID)
                                     return Boolean is

   begin -- Is_Compatable_With_ASCII

      case Character_Set is

         when CS_US_ASCII =>
            return True;

         when CS_ISO_8859_1 .. CS_ISO_8859_10 =>
            return True;

         when CS_ISO_8859_13 .. CS_ISO_8859_16 =>
            return True;

         when CS_UTF_8 =>
            return True;

         when others =>
            return False;

      end case;

   end Is_Compatable_With_ASCII;

end Basil.Strings;
