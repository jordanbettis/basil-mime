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

with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Strings.Maps;        use Ada.Strings.Maps;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Characters.Latin_1;

with Basil.Strings;           use Basil.Strings;
with Basil.Tokens;            use Basil.Tokens;
with Basil.Utils;             use Basil.Utils;
with Basil.Automata;          use Basil.Automata;

package body Basil.Headers is

   --  This constant is used several times in this package to set a
   --  limit on the maximum length of a header. It is defined by RFC
   --  2822. (p 6). Headers created using this library are not
   --  permitted to exceed this length limit.
   Optimal_Header_Length : constant Positive := 998;

   --  The Optimal_Header_Length only applies to headers created using
   --  this library. We relax the limits for messages produced
   --  externally and parsed using Basil. However, allowing a header
   --  to be arbitrarly large may allow an attacker to produce very
   --  long and complex structured headers, which could cause resource
   --  exhaustion in the structured header parsers. This generous
   --  limit is chosen arbitrarily to allow "good faith" consumption
   --  of malformed headers while preventing that potential attack
   --  vector.
   Maximum_Header_Length  : constant Positive := 5000;

   ----------------------------------------------------------------------------
   --  This function takes a value and returns a representation
   --  wrapped to attempt to conform to the 78 character line limit
   --  from RFC2822. Key_Length is the length of the key, which is
   --  needed to determine how long the first line should be.
   --
   --  It's not an error to exceed 78 characters if the function can
   --  not find a place to wrap.
   function Fold_Header_Value (Value      : in Encoded_String;
                               Key_Length : in Natural)
                              return Encoded_String;

   ----------------------------------------------------------------------------
   --  This character is for validating input for invalid
   --  characters. It tests if a given string has any characters in a
   --  given set of illegal values. If *any* characters in the input
   --  string are in that set, then it returns true.
   function Has_Character_In (Value  : in Encoded_String;
                              Set    : in Character_Set)
                             return Boolean;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function From_String (Input : in String;
                         Index : in Positive := 1)
                        return Header is

      Working_Index : Positive := Index;
      New_Header    : Header;

   begin -- From_String

      From_String (New_Header => New_Header,
                   Input      => Input,
                   Index      => Working_Index);

      return New_Header;

   end From_String;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure From_String (New_Header :    out Header;
                          Input      : in     String;
                          Index      : in out Positive) is

      package L renames Ada.Characters.Latin_1;

      Key_End    : Natural;
      Value_End  : Natural;

   begin -- From_String

      if Index > Input'Length then
         raise Premature_End;
      end if;

      --  If we're consuming headers and come upon a single newline
      --  sequence on the input, that means that we have arrived at
      --  the double newline that terminates the list, as each line
      --  consumes its trailing newline.
      if Input(Index) = L.CR or Input(Index) = L.LF then

         New_Header.Key   := US("");
         New_Header.Value := US("");

         Index := Index + 1;

         if Index <= Input'Length and then
           (Input(Index - 1) = L.CR and Input(Index) = L.LF)
         then
            Index := Index + 1;
         end if;

         return;

      end if;

      --  RFC 2822 defines keys as consisting of the set of characters
      --  in RFC2235_Printable. For this operation we expand the
      --  definition to include high-bit characters as well.
      Key_End := Get_Token_End (Source => Input,
                                First  => Index,
                                Set    =>
                                  (RFC2234_Visibles or
                                   Highbit_Printable)
                                  and (not To_Set(":")));

      if Key_End /= 0 then
         New_Header.Key := US (Input(Index .. Key_End));
         Index := Key_End + 1;
      else
         raise Invalid_Header;
      end if;

      if Input(Index) = ':' then
         Index := Index + 1;
      else
         raise Invalid_Header;
      end if;

      Value_End := Automata.Get_Token_End (Source     => Input,
                                           First      => Index,
                                           Automation => Header_Value);

      if Value_End /= 0
        and then (Value_End - Index) < Maximum_Header_Length
      then
         New_Header.Value := US (Input(Index .. Value_End));
         Index := Value_End + 1;
      else
         raise Invalid_Header;
      end if;

      -- Remove leading whitespace and trailing newline from the value
      Trim (New_Header.Value,
            Left  => Whitespace,
            Right => Newlines);

   end From_String;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function To_String (Input_Header : in Header) return String is

      package L renames Ada.Characters.Latin_1;

   begin -- To_String

      return S (Input_Header.Key & US (": ") & Input_Header.Value &
                US (L.CR & L.LF));

   end To_String;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function To_Header (Key   : in Encoded_String;
                       Value : in Encoded_String)
                      return Header is

      Working_Value         : Unbounded_String := EUS (Value);
      New_Header            : Header;

   begin -- To_Header

      if Has_Character_In (Key, not (RFC2234_Visibles xor To_Set(":")))
        or Has_Character_In (Value, not (RFC2822_Text))
      then
        raise Invalid_Character;
      end if;

      declare

         Folded_Value : Encoded_String := Fold_Header_Value (Value,
                                                             Key'Length);

      begin

         if Key'Length + 2 + Folded_Value'Length > Optimal_Header_Length then
            raise Invalid_Header;
         end if;

         New_Header.Key   := EUS (Key);
         New_Header.Value := EUS (Folded_Value);

      end;

      return New_Header;

   end To_Header;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_Value (Input_Header : in Header) return String is

   begin -- Get_Value

      return S (Input_Header.Value);

   end Get_Value;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_Key   (Input_Header : in Header) return String is

   begin -- Get_Key

      return S (Input_Header.Key);

   end Get_Key;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Set_Value (Target_Header : in out Header;
                        Value         : in     Encoded_String) is

   begin -- Set_Value

      if Has_Character_In (Value, not (RFC2822_Text)) then
         raise Invalid_Character;
      end if;

      declare

         Folded_Value : Encoded_String :=
           Fold_Header_Value (Value, Length (Target_Header.Key));

      begin

         if Length (Target_Header.Key) + 2 +
           Folded_Value'Length > Optimal_Header_Length
         then
            raise Invalid_Header;
         end if;

         Target_Header.Value := EUS (Folded_Value);

      end;

   end Set_Value;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Set_Key   (Target_Header : in out Header;
                        Key           : in     Encoded_String) is

   begin -- Set_Key

      if Has_Character_In (Key, not (RFC2234_Visibles xor To_Set(":"))) then
        raise Invalid_Character;
      end if;

      Target_Header.Key := EUS (Key);

   end Set_Key;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Fold_Header_Value (Value      : in Encoded_String;
                               Key_Length : in Natural)
                              return Encoded_String is

      package L renames Ada.Characters.Latin_1;

      Working_Value   : Unbounded_String;
      Last_Whitespace : Natural           := 0;
      Last_Fold       : Positive          := 1;
      Index           : Positive          := Value'First;
      Column          : Positive          := Key_Length + 2;
      Fold_Length     : constant Positive := 78;
      Escaping        : Boolean           := False;

   begin -- Fold_Header_Value

      while Index <= Value'Length loop

         --  We don't want to break on escaped whitespace, so we have
         --  a little state machine to protect against it. We only
         --  index the whitespace as a possible break point if we're
         --  not in the escaping state.
         if Escaping then

            Escaping := False;

         else

            if Is_In (Value(Index), RFC2234_Whitespace) then
               Last_Whitespace := Index;
            elsif Value(Index) = '\' then
               Escaping := True;
            end if;

         end if;

         if Column >= Fold_Length and Last_Whitespace /= 0 then
            Append (Working_Value,
                    EUS (Value(Last_Fold .. Last_Whitespace - 1))
                    & US (L.CR & L.LF));
            Last_Fold       := Last_Whitespace;
            Last_Whitespace := 0;
            Column          := Index - (Last_Fold - 1);
         end if;

         Index  := Index  + 1;
         Column := Column + 1;

      end loop;

      return ES (Working_Value) & Value(Last_Fold .. Value'Length);

   end Fold_Header_Value;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Has_Character_In (Value  : in Encoded_String;
                              Set    : in Character_Set)
                             return Boolean is

   begin -- Has_Character_In

      for I in Value'Range loop
         if Is_In (Value(I), Set) then
            return True;
         end if;
      end loop;

      return False;

   end Has_Character_In;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Are_Equal (Left, Right : Header)
                      return Boolean is

   begin -- Are_Equal

      -- Header keys are not case sensitive
      if To_Upper (S (Left.Key)) = To_Upper (S (Right.Key)) then
         return True;
      else
         return False;
      end if;

   end Are_Equal;

end Basil.Headers;
