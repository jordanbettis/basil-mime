-----------------------------------------------------------------------------
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
with Ada.Characters.Latin_1;     use Ada.Characters.Latin_1;

with Basil.Tokens;               use Basil.Tokens;

package body Basil.Utils is

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Replace_All (Source_String : in out Unbounded_String;
                          Target        : in     String;
                          Replacement   : in     String) is

      Index_Of_Pattern : Integer;

   begin -- Replace_All

      Index_Of_Pattern := Index( Source_String, Target);

      while Index_Of_Pattern /= 0 loop

         Replace_Slice (Source_String,
                        Index_Of_Pattern,
                        (Index_Of_Pattern + Target'Length - 1),
                        Replacement);
         Index_Of_Pattern := Index( Source_String, Target);

      end loop;

   end Replace_All;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Enquote_If_Needed (Source           : in String;
                               Requires_Quoting : in Character_Set;
                               Add_Quotation    : in Boolean := True)
                              return String is

      package L renames Ada.Characters.Latin_1;

      Escaped           : Boolean        := False;
      Working_String    : Unbounded_String;

   begin -- Enuqote_If_Needed

      for I in Source'Range loop

         if Is_In (Source(I), Requires_Quoting) then
            Append (Working_String, '\' & Source(I));
            Escaped := True;
         else
            Append (Working_String, Source(I));
         end if;

      end loop;

      if Escaped and Add_Quotation then
         return L.Quotation & To_String (Working_String) & L.Quotation;
      else
         return S (Working_String);
      end if;

   end Enquote_If_Needed;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Unenquote_If_Needed (Source        : in String;
                                 Has_Quotation : in Boolean := True)
                                return String is

      package L renames Ada.Characters.Latin_1;

      Working_String : Unbounded_String;
      Escaping       : Boolean := False;
      Offset         : Natural;

   begin -- Unenquote_If_Needed

      if Source'Length < 1 then
         return Source;
      end if;

      if Has_Quotation and (Source(Source'First) /= L.Quotation or
                            Source(Source'Last) /= L.Quotation)
      then
         return Source;
      end if;

      -- If Has_Quotation then we want to skip the first and last
      --  characters, because they're the '"' marks.
      if Has_Quotation then
         Offset := 1;
      else
         Offset := 0;
      end if;

      for I in Source'First + Offset .. Source'Last - Offset loop

         if Source(I) = '\' and not Escaping then
            Escaping := True;
         elsif Escaping then
            Escaping := False;
            Append (Working_String, Source(I));
         else
            Append (Working_String, Source(I));
         end if;

      end loop;

      return S (Working_String);

   end Unenquote_If_Needed;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Enquote_Domain (Source : in String) return String is

   begin -- Enquote_Domain

      if Source'Length > 2 and then
        (Source (Source'First) = '[' and Source (Source'Last) = ']')
      then

         return '['
           & Enquote_If_Needed (Source (Source'First + 1 .. Source'Last - 1),
                                Requires_Quoting => not (RFC2822_Domain_Text
                                                         or To_Set (".")),
                                Add_Quotation    => False)
           & ']';

      else
         return Source;
      end if;

   end Enquote_Domain;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Unenquote_Domain (Source : in String) return String is

   begin -- Unenquote_Domain

      if Source'Length > 2 and then
        (Source (Source'First) = '[' and Source (Source'Last) = ']')
      then

         return '['
           & Unenquote_If_Needed (Source (Source'First + 1 .. Source'Last - 1),
                                 Has_Quotation    => False)
           & ']';

      else
         return Source;
      end if;

   end Unenquote_Domain;

end Basil.Utils;
