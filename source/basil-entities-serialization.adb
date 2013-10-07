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
with Ada.Containers;          use Ada.Containers;
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;

with Basil.Headers;           use Basil.Headers;
with Basil.Strings;           use Basil.Strings;
with Basil.Utils;             use Basil.Utils;
with Basil.Tokens;            use Basil.Tokens;
with Basil.Automata;          use Basil.Automata;

package body Basil.Entities.Serialization is

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Serialize_Entity (Source : in Abstract_Entity'Class)
                             return String is

      package L renames Ada.Characters.Latin_1;

      -------------------------------------------------------------------------
      procedure Validate_Content_Type (Source : in Abstract_Entity'Class);

      procedure Validate_Content_Type (Source : in Abstract_Entity'Class) is

         Type_From_Headers : Content_Type
           := From_Headers (Source.Headers_List);

      begin -- Validate_Content_Type

         if Get_Content_Class (Source.Entity_Type) /= Unknown
           and Source.Entity_Type.Content_Transfer_Encoding /= CTE_Unknown
         then

            if Type_From_Headers /= Source.Entity_Type then
               raise Invalid_Content_Type;
            end if;

         end if;

      end Validate_Content_Type;

      -------------------------------------------------------------------------
      function Get_Children (Source : in Abstract_Entity'Class)
                            return String;

      function Get_Children (Source : in Abstract_Entity'Class)
                            return String is

         Delimiter : Unbounded_String
           := Source.Entity_Type.Multipart_Delimiter;
         Current   : Child_Cursor     := First_Child (Source, Strategy_Simple);
         Value     : Unbounded_String;

      begin -- Get_Children

         if Length (Delimiter) < 1 then
            Delimiter := US (Generate_Multipart_Delimiter);
         end if;

         while Has_Child (Current) loop

            Append (Value, L.CR & L.LF & "--" & S (Delimiter) & L.CR & L.LF
                    & Serialize_Entity (Get_Child (Current)));

            Next_Child (Current);

         end loop;

         Append (Value,
                 L.CR & L.LF & "--" & S (Delimiter) & "--" & L.CR & L.LF);

         return S (Value);

      end Get_Children;

      -------------------------------------------------------------------------
      Serialized_Headers : Unbounded_String;
      Body_Part          : Unbounded_String;
      Trailing_Junk      : Unbounded_String;
      Header_Cursor      : Headers.Lists.Cursor
        := Headers.Lists.First (Source.Headers_List);

   begin -- Serialize_Entity

      Validate_Content_Type (Source);

      while Headers.Lists.Has_Element (Header_Cursor) loop

         Append (Serialized_Headers,
                 Headers.To_String (Headers.Lists.Element (Header_Cursor)));
         Headers.Lists.Next (Header_Cursor);

      end loop;

      if Source.Entity_Type.Content_Transfer_Encoding = CTE_7bit or
        Source.Entity_Type.Content_Transfer_Encoding = CTE_Quoted_Printable or
        Source.Entity_Type.Content_Transfer_Encoding = CTE_Base64
      then

         Body_Part     :=
           US (Ensure_Message_Newlines (S (Source.Body_Part)));
         Trailing_Junk :=
           US (Ensure_Message_Newlines (S (Source.Trailing_Junk)));

      else

         Body_Part     := Source.Body_Part;
         Trailing_Junk := Source.Trailing_Junk;

      end if;

      if Is_Multipart (Source) then

         if Body_Part /= Null_Unbounded_String then

            return S (Serialized_Headers) & L.CR & L.LF & S (Body_Part)
              & Get_Children (Source) & S (Trailing_Junk);

         else

            return S (Serialized_Headers)  & Get_Children (Source)
              & S (Trailing_Junk);

         end if;

      else

         return S (Serialized_Headers) & L.CR & L.LF & S (Body_Part);

      end if;

   end Serialize_Entity;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Unserialize_Entity (Source       : in     String;
                                 Delimiter    : in     String   := "";
                                 Index        : in out Positive;
                                 Entity       :    out Abstract_Entity'Class;
                                 Get_Children : not null access procedure
                                 (Source : in     String;
                                  Index  : in out Positive;
                                  Entity : in out Abstract_Entity'Class)) is

      -------------------------------------------------------------------------
      procedure Get_Body (Output    : in out Unbounded_String;
                          Index     : in out Positive;
                          Source    : in     String;
                          Delimiter : in     String);

      procedure Get_Body (Output    : in out Unbounded_String;
                          Index     : in out Positive;
                          Source    : in     String;
                          Delimiter : in     String) is

         package L renames Ada.Characters.Latin_1;

         Index_End : Natural := 0;

      begin -- Get_Body

         if Delimiter = "" then
            Index_End := Source'Last;

         else

            Index_End := Ada.Strings.Fixed.Index
              (Source  => Source,
               Pattern => "--" & Delimiter,
               From    => Index);

            if Index_End = 0 then
               raise Invalid_Message_Or_Entity;
            end if;

         end if;

         Output := US (Source (Index .. Index_End - 1));

         if Length (Output) >= 4 and then
           Tail (Output, 4) = US (L.CR & L.LF & L.CR & L.LF)
         then
            Head (Output, Length (Output) - 2);
         elsif Length (Output) >= 2 and then
           (Tail (Output, 2) = US (L.CR & L.CR) or
            Tail (Output, 2) = US (L.LF & L.LF))
         then
            Head (Output, Length (Output) - 1);
         end if;

         if Trim (Output, Whitespace, Null_Set) = Null_Unbounded_String then
            Output := Null_Unbounded_String;
         end if;

         Index := Index_End + Delimiter'Length + 2;

         Index_End := Automata.Get_Token_End (Source     => Source,
                                              First      => Index,
                                              Automation => Newline_Sequence);

         if Index_End /= 0 then
            Index := Index_End + 1;
         end if;

      end Get_Body;

      -------------------------------------------------------------------------
      Current_Header : Header;
      Index_End : Natural := 0;

   begin -- Unserialize_Entity

      if Index > Source'Last then
         raise Invalid_Message_Or_Entity;
      end if;

      From_String (New_Header => Current_Header,
                   Input      => Source,
                   Index      => Index);

      while Current_Header /= Empty_Header loop

         Headers.Lists.Append (Entity.Headers_List, Current_Header);
         From_String (New_Header => Current_Header,
                      Input      => Source,
                      Index      => Index);

      end loop;

      if Headers.Lists.Length (Entity.Headers_List) = 0 then

         Index_End := Automata.Get_Token_End
           (Source     => Source,
            First      => Index,
            Automation => Newline_Sequence);

         if Index_End /= 0 then
            Index := Index_End + 1;
         end if;

         --  If the entity has no headers it is assumed to be
         --  text/plain with CTE=7bit (RFC 2049 p. 15)
         Entity.Entity_Type :=
           Content_Type'(MIME_Type     => T_Text,
                         Text_Subtype  => ST_Plain,
                         Character_Set => CS_US_ASCII,
                         Content_Transfer_Encoding =>
                           CTE_7bit);

      else

         Entity.Entity_Type := From_Headers (Entity.Headers_List);

      end if;

      if Entity.Entity_Type.MIME_Type = T_Multipart then

         if Length (Entity.Entity_Type.Multipart_Delimiter) < 1 then
            raise Invalid_Message_Or_Entity;
         end if;

         Get_Body (Output    => Entity.Body_Part,
                   Source    => Source,
                   Index     => Index,
                   Delimiter => S (Entity.Entity_Type.Multipart_Delimiter));

         Get_Children (Source => Source,
                       Index  => Index,
                       Entity => Entity);

         Index_End := Automata.Get_Token_End
           (Source     => Source,
            First      => Index,
            Automation => Newline_Sequence);

         if Index_End /= 0 then
            Index := Index_End + 1;
         end if;

         Get_Body (Output    => Entity.Trailing_Junk,
                   Source    => Source,
                   Index     => Index,
                   Delimiter => Delimiter);


      else

         Get_Body (Output    => Entity.Body_Part,
                   Source    => Source,
                   Index     => Index,
                   Delimiter => Delimiter);

      end if;

   end Unserialize_Entity;

end Basil.Entities.Serialization;
