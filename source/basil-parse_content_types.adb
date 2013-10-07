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

with Ada.Strings.Maps;         use Ada.Strings.Maps;
with Ada.Characters.Latin_1;

with Basil.Utils;              use Basil.Utils;
with Basil.Tokens;             use Basil.Tokens;
with Basil.Automata;           use Basil.Automata;

with Ada.Text_IO;              use Ada.Text_IO;

package body Basil.Parse_Content_Types is

   --  This parser matches a modification of the EBNF in RFC 2045 p
   --  12. The modifications allow for the inclusion of whitespace and
   --  comments between most tokens, as that seems to be the intent of
   --  the standard and to fullfill 'best effort' parsing.
   --
   --  In addition, this parser only recieves the value of the header
   --  line, while the EBNF describes the whole thing, and it is not
   --  necessary for this parser to distinguish between type names and
   --  x-tokens, as that will be handled elsewhere.
   --
   --  This is the modified EBNF matched by this parser.
   --
   --  content   := CWS type CWS "/" CWS subtype CWS *(parameter)
   --  type      := token
   --  subtype   := token
   --  parameter := FWS ; FWS attribute FWS value
   --  attribute := token
   --  value     := token / quoted_string
   --  CWS       := *(whitespace / comment) ; see Automata.Uninteresting

   ----------------------------------------------------------------------------
   --  Each of these functions match tokens. Get_Type_Label matches
   --  both type and subtype labels, as they're both just 'tokens.'
   --  Get_Parameter returns an empty header object if it encounters
   --  the end of the string before finding a parameter;
   procedure Get_Type_Label      (Value           : in out Unbounded_String;
                                  Type_Label      :    out Unbounded_String);

   procedure Get_Parameter       (Value           : in out Unbounded_String;
                                  Output          :    out Parameter);

   procedure Get_Parameter_Value (Value           : in out Unbounded_String;
                                  Parameter_Value :    out Unbounded_String);

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Parse_Content_Type (Header_Value : in Encoded_String)
                               return Content_Type_Semantics is

      Working_Value     : Unbounded_String := EUS (Header_Value);
      Working_Token     : Unbounded_String;
      Working_Parameter : Parameter;

      Semantics         : Content_Type_Semantics;

   begin -- Parse_Content_Type

      Automata.Discard_Token (Working_Value, Uninteresting);

      Get_Type_Label (Working_Value,
                      Working_Token);
      Semantics.MIME_Type := Working_Token;

      Discard_Token (Working_Value, Uninteresting);

      Get_Token (Source => Working_Value,
                 Token  => Working_Token,
                 Set    => To_Set("/"));

      if Working_Token /= "/" then
         raise Parse_Error;
      end if;

      Automata.Discard_Token (Working_Value, Uninteresting);

      Get_Type_Label (Working_Value,
                      Working_Token);
      Semantics.Sub_Type := Working_Token;

      Get_Parameter (Working_Value, Working_Parameter);

      while Working_Parameter /= Empty_Parameter loop

         Parameter_Lists.Append (Semantics.Parameters, Working_Parameter);
         Get_Parameter (Working_Value, Working_Parameter);

      end loop;

      return Semantics;

   end Parse_Content_Type;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Get_Type_Label (Value      : in out Unbounded_String;
                             Type_Label :    out Unbounded_String) is

   begin -- Get_Type_Label

      Get_Token (Source => Value,
                 Token  => Type_Label,
                 Set    => RFC2045_Token);

      if Type_Label = "" then
         raise Parse_Error;
      end if;

   end Get_Type_Label;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Get_Parameter  (Value  : in out Unbounded_String;
                             Output :    out Parameter) is

      Working_Token : Unbounded_String;

   begin -- Get_Parameter

      Automata.Discard_Token (Value, Uninteresting);

      Get_Token (Source => Value,
                 Token  => Working_Token,
                 Set    => To_Set(";"));

      if Working_Token /= ";" then
         Output.Key   := Null_Unbounded_String;
         Output.Value := Null_Unbounded_String;
         return;
      end if;

      Automata.Discard_Token (Value, Uninteresting);

      Get_Token (Source => Value,
                 Token  => Working_Token,
                 Set    => RFC2045_Token);
      Output.Key := Working_Token;

      if Working_Token = "" then
         raise Parse_Error;
      end if;

      Automata.Discard_Token (Value, Uninteresting);

      if S (Head (Value, 1)) = "=" then
         Tail (Value, Length (Value) - 1);
      else
         raise Parse_Error;
      end if;

      Automata.Discard_Token (Value, Uninteresting);

      Get_Parameter_Value (Value, Working_Token);
      Output.Value := Working_Token;

   end Get_Parameter;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Get_Parameter_Value (Value           : in out Unbounded_String;
                                  Parameter_Value :    out Unbounded_String) is

      package L renames Ada.Characters.Latin_1;

      Working_Token : Unbounded_String;

   begin -- Get_Parameter_Value

      if S (Head (Value, 1)) =  String'(1 .. 1 => L.Quotation) then

         Get_Token (Source => Value,
                    Token  => Working_Token,
                    Set    => RFC2045_Token or To_Set (L.Quotation),
                    Quoted => True);

         Parameter_Value := US (Unenquote_If_Needed (S (Working_Token)));

      else

         Get_Token (Source => Value,
                    Token  => Working_Token,
                    Set    => RFC2045_Token);
         Parameter_Value := Working_Token;

      end if;

   end Get_Parameter_Value;

end Basil.Parse_Content_Types;
