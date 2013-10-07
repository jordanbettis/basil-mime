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

with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Strings.Maps;           use Ada.Strings.Maps;
with Ada.Characters.Latin_1;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Bounded;

with Basil.Character_Set_Translation;
use  Basil.Character_Set_Translation;

with Basil.Utils;                use Basil.Utils;
with Basil.Strings;              use Basil.Strings;
with Basil.Headers;              use Basil.Headers;
with Basil.Tokens;               use Basil.Tokens;
with Basil.Automata;             use Basil.Automata;
with Basil.Parse_Content_Types;  use Basil.Parse_Content_Types;

package body Basil.Content_Types is


   ----------------------------------------------------------------------------
   --  This function translates the enumeration labels for the MIME
   --  types or content transfer encodings (but NOT character sets)
   --  into strings for inclusion in serialized content type stanzas
   function Serialize_Label (Identifier : String)
                            return String;

   ----------------------------------------------------------------------------
   --  This stuff is used by Generate_Multipart_Delimiter below. It is
   --  defined here at the package level so that we can prime Randomer
   --  once per program invocation rather than once per function call,
   --  which can result in multiple identical delimiters if it is
   --  called quickly. See basil-message_id_headers.adb for more
   --  information.
   Pick_Array       : constant String   := "ABCDEFJHIJKLMNOPQRSTUVWXYZ";
   Delimiter_Length : constant Positive := 20;

   subtype Random_Index is Integer range 1 .. Pick_Array'Length;
   package Picker is new Ada.Numerics.Discrete_Random (Random_Index);
   package Bounded is
      new Ada.Strings.Bounded.Generic_Bounded_Length (Delimiter_Length);

   --  This generator is primed in the body of this package
   Randomer   : Picker.Generator;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function From_Headers (Source : in Headers.Lists.List)
                         return Content_Type is

      -------------------------------------------------------------------------
      function Get_MIME_Type (Parsed : Content_Type_Semantics)
                             return Primary_Content_Type;

      function Get_MIME_Type (Parsed : Content_Type_Semantics)
                             return Primary_Content_Type is

         Label : String := To_Upper (S (Parsed.MIME_Type));

      begin -- Get_MIME_Type

         if Label = "APPLICATION" then
            return T_Application;
         elsif Label = "AUDIO" then
               return T_Audio;
         elsif Label = "IMAGE" then
            return T_Image;
         elsif Label = "MESSAGE" then
            return T_Message;
         elsif Label = "MODEL" then
            return T_Model;
         elsif Label = "MULTIPART" then
            return T_Multipart;
         elsif Label = "TEXT" then
            return T_Text;
         elsif Label = "VIDEO" then
            return T_Video;
         else
            return T_Unknown;
         end if;

      end Get_MIME_Type;

      -----------------------------------------------------------------
      --  This generic function returns the MIME Subtype enumeration
      --  of the given type that matches the label in the given
      --  semantics object. Unknown is the value that shall be
      --  returned if none of the enumerations mach the label
      generic
         type Subtype_Enumeration is ( <> );
      function Get_Generic_Type (Parsed  : Content_Type_Semantics;
                                 Unknown : Subtype_Enumeration)
                                 return Subtype_Enumeration;

      function Get_Generic_Type (Parsed  : Content_Type_Semantics;
                                 Unknown : Subtype_Enumeration)
                                 return Subtype_Enumeration is

      begin -- Get_Generic_Type

         for I in Subtype_Enumeration'Range loop

            if Serialize_Label
              (Subtype_Enumeration'Image(Subtype_Enumeration(I))) =
              To_Upper (S (Parsed.Sub_Type))
            then
               return Subtype_Enumeration(I);
            end if;

         end loop;

         return Unknown;

      end Get_Generic_Type;

      function Get_Application_Type is new Get_Generic_Type
        (Subtype_Enumeration => Application_Content_Subtype);

      function Get_Audio_Type is new Get_Generic_Type
        (Subtype_Enumeration => Audio_Content_Subtype);

      function Get_Image_Type is new Get_Generic_Type
        (Subtype_Enumeration => Image_Content_Subtype);

      function Get_Message_Type is new Get_Generic_Type
        (Subtype_Enumeration => Message_Content_Subtype);

      function Get_Model_Type is new Get_Generic_Type
        (Subtype_Enumeration => Model_Content_Subtype);

      function Get_Multipart_Type is new Get_Generic_Type
        (Subtype_Enumeration => Multipart_Content_Subtype);

      function Get_Text_Type is new Get_Generic_Type
        (Subtype_Enumeration => Text_Content_Subtype);

      function Get_Video_Type is new Get_Generic_Type
        (Subtype_Enumeration => Video_Content_Subtype);

      -------------------------------------------------------------------------
      function Get_Character_Set (Parsed : Content_Type_Semantics)
                                 return Character_Set_ID;

      function Get_Character_Set (Parsed : Content_Type_Semantics)
                                 return Character_Set_ID is

         package PL renames Parse_Content_Types.Parameter_Lists;

         Parameter_Cursor    : PL.Cursor  := PL.First (Parsed.Parameters);

      begin -- Get_Character_Set

         while PL.Has_Element (Parameter_Cursor) loop

            if To_Upper
              (S (PL.Element (Parameter_Cursor).Key)) = "CHARSET"
            then
               return Get_Identifier (S (PL.Element (Parameter_Cursor).Value));
            end if;

            PL.Next (Parameter_Cursor);

         end loop;

         -- if no charset parameter is present we default
         -- to US-ASCII per RFC 2045
         return CS_US_ASCII;

      end Get_Character_Set;

      -------------------------------------------------------------------------
      function Get_Multipart_Delimiter (Parsed : Content_Type_Semantics)
                                       return String;

      function Get_Multipart_Delimiter (Parsed : Content_Type_Semantics)
                                       return String is

         package PL renames Parse_Content_Types.Parameter_Lists;

         Parameter_Cursor    : PL.Cursor  := PL.First (Parsed.Parameters);

      begin -- Get_Multipart_Delimiter

         while PL.Has_Element (Parameter_Cursor) loop

            if To_Upper
              (S (PL.Element (Parameter_Cursor).Key)) = "BOUNDARY"
            then
               return S (PL.Element (Parameter_Cursor).Value);
            end if;

            PL.Next (Parameter_Cursor);

         end loop;

         return "";

      end Get_Multipart_Delimiter;

      -------------------------------------------------------------------------
      function New_Content_Type (Header_Value  : in Unbounded_String)
                                return Content_Type;

      function New_Content_Type (Header_Value  : in Unbounded_String)
                                return Content_Type is

         Parsed : Content_Type_Semantics;

      begin -- New_Content_Type

         Parsed := Parse_Content_Type (ES (Header_Value));

         if To_Upper (S (Head (Parsed.MIME_Type, 2))) = "X-" then
            return Content_Type'(MIME_Type                 => T_Experimental,
                                 Experimental_Subtype      => ST_Experimental,
                                 Content_Transfer_Encoding => CTE_7bit);
         end if;

         declare

            Output_Object : Content_Type
              (MIME_Type => Get_MIME_Type (Parsed));

         begin

            case Output_Object.MIME_Type is

               when T_Application =>
                  Output_Object.Application_Subtype
                    := Get_Application_Type (Parsed, ST_Unknown);

               when T_Audio =>
                  Output_Object.Audio_Subtype :=
                    Get_Audio_Type (Parsed, ST_Unknown);

               when T_Image =>
                  Output_Object.Image_Subtype :=
                    Get_Image_Type (Parsed, ST_Unknown);

               when T_Message =>
                  Output_Object.Message_Subtype :=
                    Get_Message_Type (Parsed, ST_Unknown);

               when T_Model =>
                  Output_Object.Model_Subtype :=
                    Get_Model_Type (Parsed, ST_Unknown);

               when T_Multipart =>
                  Output_Object.Multipart_Subtype :=
                    Get_Multipart_Type (Parsed, ST_Unknown);
                  Output_Object.Multipart_Delimiter
                    := US (Get_Multipart_Delimiter (Parsed));

               when T_Text =>
                  Output_Object.Text_Subtype :=
                    Get_Text_Type (Parsed, ST_Unknown);
                  Output_Object.Character_Set := Get_Character_Set (Parsed);

               when T_Video =>
                  Output_Object.Video_Subtype :=
                    Get_Video_Type (Parsed, ST_Unknown);

               when others =>
                  raise Invalid_Content_Type;

            end case;

            return Output_Object;

         end;

      end New_Content_Type;

      -------------------------------------------------------------------------
      procedure Set_CTE_Element (Output_Object : in out Content_Type;
                                 Header_Value  : in     Unbounded_String);

      procedure Set_CTE_Element (Output_Object : in out Content_Type;
                                 Header_Value  : in     Unbounded_String) is

         Value : Unbounded_String := Header_Value;
         Label : Unbounded_String;

      begin -- Set_Type_Elements

         Automata.Discard_Token (Value, Uninteresting);
         Get_Token (Source => Value,
                    Token  => Label,
                    Set    => RFC2045_Token);

         declare

            S_Label : String := To_Upper (S (Label));

         begin

            if S_Label = "7BIT" then
               Output_Object.Content_Transfer_Encoding := CTE_7bit;
            elsif S_Label = "8BIT" then
               Output_Object.Content_Transfer_Encoding := CTE_8bit;
            elsif S_Label = "BINARY" then
               Output_Object.Content_Transfer_Encoding := CTE_Binary;
            elsif S_Label = "QUOTED-PRINTABLE" then
               Output_Object.Content_Transfer_Encoding := CTE_Quoted_Printable;
            elsif S_Label = "BASE64" then
               Output_Object.Content_Transfer_Encoding := CTE_Base64;
            else
               if S_Label(S_Label'First .. S_Label'First + 2) = "X-" then
                  Output_Object.Content_Transfer_Encoding := CTE_Experimental;
               else
                  Output_Object.Content_Transfer_Encoding := CTE_Unknown;
               end if;
            end if;

         end;

      end Set_CTE_Element;

      -------------------------------------------------------------------------
      CT_Value      : Unbounded_String;
      CTE_Value     : Unbounded_String;

      Header_Cursor : Headers.Lists.Cursor := First (Source);
      Output_Object : Content_Type;

   begin -- From_Headers

      while Has_Element (Header_Cursor) loop

         if To_Upper (Get_Key (Element (Header_Cursor))) = "CONTENT-TYPE" then

            if Length (CT_Value) = 0 then
               CT_Value := US (Get_Value (Element (Header_Cursor)));
            else
               raise Invalid_Content_Type;
            end if;

         elsif To_Upper (Get_Key (Element (Header_Cursor)))
           = "CONTENT-TRANSFER-ENCODING"
         then

            if Length (CTE_Value) = 0 then
               CTE_Value := US (Get_Value (Element (Header_Cursor)));
            else
               raise Invalid_Content_Type;
            end if;

         end if;

         Next (Header_Cursor);

      end loop;

      --  Per the standard (RFC 2049 p. 15) any entity which lacks
      --  content type headers is to be considered text/plain with a
      --  character set of US-ASCII and the 7Bit transfer encoding.
      if Length (CTE_Value) = 0 then
         CTE_Value := US ("7BIT");
      end if;

      if Length (CT_Value) = 0 then
         CT_Value := US ("TEXT/PLAIN; CHARSET=US-ASCII");
      end if;

      Output_Object := New_Content_Type (Header_Value => CT_Value);

      Set_CTE_Element   (Output_Object => Output_Object,
                         Header_Value  => CTE_Value);

      return Output_Object;

   end From_Headers;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function To_Headers (Source      : in Content_Type;
                        Add_Version : in Boolean)
                      return Headers.Lists.List is

      -------------------------------------------------------------------------
      function Make_Content_Type_Line (MIME_Type           : in String;
                                       MIME_Subtype        : in String;
                                       Character_Set       : in String := "";
                                       Multipart_Delimiter : in String := "")
                                      return Header;

      function Make_Content_Type_Line (MIME_Type           : in String;
                                       MIME_Subtype        : in String;
                                       Character_Set       : in String := "";
                                       Multipart_Delimiter : in String := "")
                                      return Header is

         Value : Unbounded_String;

      begin -- Make_Content_Type_Line

         Value :=
           US (MIME_Type & "/" & MIME_Subtype);

         if Character_Set /= "" then
            Value := Value & US ("; charset=" &
                                 Enquote_If_Needed (Character_Set,
                                                    Requires_Quoting =>
                                                      not RFC2045_Token));
         end if;

         if Multipart_Delimiter /= "" then
            Value := Value & US
              ("; boundary=" & Enquote_If_Needed (Multipart_Delimiter,
                                                  Requires_Quoting =>
                                                    not RFC2045_Token));
         end if;

         return To_Header ("Content-Type", ES (Value));

      end Make_Content_Type_Line;

      -------------------------------------------------------------------------
      function Get_CTE_Label (Input: Content_Transfer_Encoding_ID)
                             return Encoded_String;

      function Get_CTE_Label (Input: Content_Transfer_Encoding_ID)
                             return Encoded_String is

      begin -- Get_CTE_Label

         case Input is
            when CTE_7bit =>
               return "7bit";
            when CTE_8bit =>
               return "8bit";
            when CTE_Binary =>
               return "Binary";
            when CTE_Quoted_Printable =>
               return "Quoted-Printable";
            when CTE_Base64 =>
               return "Base64";
            when others =>
               raise Invalid_Content_Type;
         end case;

      end Get_CTE_Label;

      -------------------------------------------------------------------------
      Output              : Headers.Lists.List;
      MIME_Type_Label     : Unbounded_String;
      MIME_Subtype_Label  : Unbounded_String;
      Character_Set_Label : Unbounded_String;
      Multipart_Delimiter : Unbounded_String;

   begin -- To_String

      if Source.MIME_Type = T_Experimental
        or Source.MIME_Type = T_Unknown then
         raise Invalid_Content_Type;
      end if;

      if Source.Content_Transfer_Encoding = CTE_Experimental
        or Source.Content_Transfer_Encoding = CTE_Unknown then
         raise Invalid_Content_Type;
      end if;

      MIME_Type_Label :=
        US (Serialize_Label (Primary_Content_Type'Image(Source.MIME_Type)));

      case Source.MIME_Type is

         when T_Application =>
            MIME_Subtype_Label :=
              US (Serialize_Label (Application_Content_Subtype'Image
                   (Source.Application_Subtype)));
         when T_Audio =>
            MIME_Subtype_Label :=
              US (Serialize_Label
                  (Audio_Content_Subtype'Image (Source.Audio_Subtype)));
         when T_Image =>
            MIME_Subtype_Label :=
              US (Serialize_Label
                  (Image_Content_Subtype'Image (Source.Image_Subtype)));
         when T_Message =>
            MIME_Subtype_Label :=
              US (Serialize_Label
                  (Message_Content_Subtype'Image (Source.Message_Subtype)));
         when T_Model =>
            MIME_Subtype_Label :=
              US (Serialize_Label
                  (Model_Content_Subtype'Image (Source.Model_Subtype)));
         when T_Multipart =>
            MIME_Subtype_Label :=
              US (Serialize_Label
                  (Multipart_Content_Subtype'Image
                   (Source.Multipart_Subtype)));
         when T_Text =>
            MIME_Subtype_Label :=
              US (Serialize_Label
                  (Text_Content_Subtype'Image (Source.Text_Subtype)));
         when T_Video =>
            MIME_Subtype_Label :=
              US (Serialize_Label
                  (Video_Content_Subtype'Image (Source.Video_Subtype)));
         when others =>
            raise Invalid_Content_Type;

      end case;

      if To_Upper (S (MIME_Subtype_Label)) = "EXPERIMENTAL" or
        To_Upper (S (MIME_Subtype_Label))  = "UNKNOWN"
      then
         raise Invalid_Content_Type;
      end if;

      if Source.MIME_Type = T_Multipart then

         if Length (Source.Multipart_Delimiter) < 1
           or Length (Source.Multipart_Delimiter) > 70 then
            raise Invalid_Delimiter;
         end if;

         Multipart_Delimiter := Source.Multipart_Delimiter;

      elsif Source.MIME_Type = T_Text then

         if Source.Character_Set = CS_Unknown then
            raise Invalid_Character_Set;
         end if;

         Character_Set_Label := US (Get_Label (Source.Character_Set));

      end if;

      if Add_Version then
         Append (Output, To_Header ("MIME-Version", "1.0"));
      end if;

      Append (Output,
              Make_Content_Type_Line (MIME_Type     => S (MIME_Type_Label),
                                      MIME_Subtype  => S (MIME_Subtype_Label),
                                      Character_Set => S (Character_Set_Label),
                                      Multipart_Delimiter =>
                                        S (Multipart_Delimiter)));

      Append (Output,
              To_Header ("Content-Transfer-Encoding",
                         Get_CTE_Label (Source.Content_Transfer_Encoding)));

      return Output;

   end To_Headers;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_Content_Class (Type_Object : Content_Type)
                              return Content_Class_Category is

   begin -- Get_Content_Class

      case Type_Object.MIME_Type is
         when T_Application =>
            if Type_Object.Application_Subtype = ST_Experimental then
               return Experimental;
            elsif  Type_Object.Application_Subtype = ST_Unknown then
               return Unknown;
            else
               return Normal;
            end if;

         when T_Audio =>
            if Type_Object.Audio_Subtype = ST_Experimental then
               return Experimental;
            elsif  Type_Object.Audio_Subtype = ST_Unknown then
               return Unknown;
            else
               return Normal;
            end if;

         when T_Experimental =>
            return Experimental;

         when T_Image =>
            if Type_Object.Image_Subtype = ST_Experimental then
               return Experimental;
            elsif  Type_Object.Image_Subtype = ST_Unknown then
               return Unknown;
            else
               return Normal;
            end if;

         when T_Message =>
            if Type_Object.Message_Subtype = ST_Experimental then
               return Experimental;
            elsif  Type_Object.Message_Subtype = ST_Unknown then
               return Unknown;
            else
               return Normal;
            end if;

         when T_Model =>
            if Type_Object.Model_Subtype = ST_Experimental then
               return Experimental;
            elsif  Type_Object.Model_Subtype = ST_Unknown then
               return Unknown;
            else
               return Normal;
            end if;

         when T_Multipart =>
            if Type_Object.Multipart_Subtype = ST_Experimental then
               return Experimental;
            elsif  Type_Object.Multipart_Subtype = ST_Unknown then
               return Unknown;
            else
               return Normal;
            end if;

         when T_Text =>
            if Type_Object.Text_Subtype = ST_Experimental then
               return Experimental;
            elsif  Type_Object.Text_Subtype = ST_Unknown or
              Type_Object.Character_Set = CS_Unknown then
               return Unknown;
            else
               return Normal;
            end if;

         when T_Unknown =>
            return Unknown;

         when T_Video =>
            if Type_Object.Video_Subtype = ST_Experimental then
               return Experimental;
            elsif  Type_Object.Video_Subtype = ST_Unknown then
               return Unknown;
            else
               return Normal;
            end if;

      end case;

   end Get_Content_Class;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Generate_Multipart_Delimiter return String is

         Value : Bounded.Bounded_String;

   begin -- Generate_Multipart_Delimiter

         for I in 1 .. Delimiter_Length loop
            Bounded.Append (Value, Pick_Array (Picker.Random (Randomer)));
         end loop;

         return Bounded.To_String (Value);

   end Generate_Multipart_Delimiter;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Serialize_Label (Identifier : String)
                            return String is

      Identifier_Label : Unbounded_String
        := US (Ada.Characters.Handling.To_Upper (Identifier));

   begin -- Serialize_Label

      -- Remove the st_, t_, or cte_ from the beginning
      if Head( Identifier_Label, 3) = "ST_" then
         Identifier_Label :=
           Tail (Identifier_Label, Length(Identifier_Label) - 3);
      elsif Head( Identifier_Label, 4) = "CTE_" then
         Identifier_Label :=
           Tail (Identifier_Label, Length(Identifier_Label) - 4);
      else
         Identifier_Label :=
           Tail (Identifier_Label, Length(Identifier_Label) - 2);
      end if;

      Replace_All (Identifier_Label, "_D_", ".");
      Replace_All (Identifier_Label, "_P_", "+");

      if Tail (Identifier_Label, 2) = "_P" then
         Identifier_Label :=
           Head (Identifier_Label, Length(Identifier_Label) - 2) & "+";
      end if;

      if Tail (Identifier_Label, 2) = "_D" then
         Identifier_Label :=
           Head (Identifier_Label, Length(Identifier_Label) - 2) & ".";
      end if;

      Replace_All (Identifier_Label, "_", "-");

      return S (Identifier_Label);

   end Serialize_Label;

begin -- Basil.Content_Types

   --  We Prime the random number generator used by
   --  Generate_Multipart_Delimiter here to prevent it being reset
   --  every function call.
   Picker.Reset (Randomer);

end Basil.Content_Types;
