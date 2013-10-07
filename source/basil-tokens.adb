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

with Ada.Strings.Maps;            use Ada.Strings.Maps;
with Ada.Characters.Latin_1;

with Basil.Utils;                 use Basil.Utils;

package body Basil.Tokens is

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_Token_End (Source : in String;
                           First  : in Positive;
                           Set    : in Character_Set;
                           Quoted  : in Boolean := False)
                          return Natural is

      Index    : Positive := First;
      Escaping : Boolean  := False;

   begin -- Get_Token_End

      if Index > Source'Last then
         return 0;
      end if;

      while Index <= Source'Length and then
        (Is_In (Source(Index), Set) or
         (Quoted and (Source(Index) = '\' or Escaping)))
      loop
         if Escaping = True then
            Escaping := False;
         elsif Source(Index) = '\' then
            Escaping := True;
         end if;
         Index := Index + 1;
      end loop;

      if Index = First then
         return 0;
      else
         return Index - 1;
      end if;

   end Get_Token_End;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Get_Token (Source : in out Unbounded_String;
                        Token  :    out Unbounded_String;
                        Set    : in     Character_Set;
                        Quoted : in     Boolean := False) is

      Index_End : Natural := Get_Token_End (S (Source),
                                            First  => 1,
                                            Set    => Set,
                                            Quoted => Quoted);

   begin -- Get_Token

      Token := Head (Source, Index_End);

      Tail (Source, Length (Source) - Index_End);

   end Get_Token;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Discard_Token (Source : in out Unbounded_String;
                            Set    : in     Character_Set;
                            Quoted : in     Boolean := False) is

      Index_End : Natural := Get_Token_End (S (Source),
                                            First  => 1,
                                            Set    => Set,
                                            Quoted => Quoted);

   begin -- Discard_Token

      Tail (Source, Length (Source) - Index_End);

   end Discard_Token;

end Basil.Tokens;
