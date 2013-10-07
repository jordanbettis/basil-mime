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

with Ada.Strings.Maps;             use Ada.Strings.Maps;

with Basil.Utils;                  use Basil.Utils;
with Basil.Tokens;                 use Basil.Tokens;

package body Basil.Address_Headers is

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function To_Address_Header (Key          : in Encoded_String;
                               Display_Name : in Encoded_String;
                               Local_Part   : in Encoded_String;
                               Domain_Part  : in Encoded_String;
                               Group_Label  : in Encoded_String := "")
                              return Address_Header is

   begin -- To_Address_Header

      return Address_Header'(Key => EUS (Key),
                             Display_Name => US (Unenquote_If_Needed
                                                 (S (Display_Name))),
                             Local_Part   => US (Unenquote_If_Needed
                                                 (S (Local_Part),
                                                  Has_Quotation => False)),
                             Domain_Part  => US (Unenquote_Domain
                                                 (S (Domain_Part))),
                             Group_Label  => US (Unenquote_If_Needed
                                                 (S (Group_Label))));

   end To_Address_Header;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_Key (Header : in Address_Header) return String is

   begin -- Get_Key

      return S (Header.Key);

   end Get_Key;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_Display_Name (Header : in Address_Header) return String is

   begin -- Get_Local_Part

      return S (Header.Display_Name);

   end Get_Display_Name;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_Local_Part  (Header : in Address_Header) return String is

   begin -- Get_Local_Part

      return S (Header.Local_Part);

   end Get_Local_Part;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_Domain_Part (Header : in Address_Header) return String is

   begin -- Get_Domain_Part

      return S (Header.Domain_Part);

   end Get_Domain_Part;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_Group_Label  (Header : in Address_Header) return String is

   begin -- Get_Group_Label

      return S (Header.Group_Label);

   end Get_Group_Label;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_Address  (Header : in Address_Header) return String is

      Local_Part   : String
        := Enquote_If_Needed (S (Header.Local_Part),
                              Requires_Quoting => not (RFC2822_Atom_Text or
                                                       To_Set (".")),
                              Add_Quotation    => False);
      Domain_Part  : String := Enquote_Domain (S (Header.Domain_Part));

   begin -- Get_Message_ID

      return Local_Part & "@" & Domain_Part;

   end Get_Address;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_Full_Address  (Header : in Address_Header) return String is

      -------------------------------------------------------------------------
      --  This function ensures that there is a space between the
      --  display name and the local part if a display name exists.
      function Format_Name (Name_Value : String) return String;

      function Format_Name (Name_Value : String) return String is

      begin -- Format_Name

         if Name_Value'Length /= 0 then
            return Name_Value & " ";
         else
            return "";
         end if;

      end Format_Name;

      -------------------------------------------------------------------------
      Name_Value   : String
        := Enquote_If_Needed (S (Header.Display_Name),
                              Requires_Quoting  => not (RFC2822_Atom_Text or
                                                        To_Set (" ")),
                              Add_Quotation     => True);
      Local_Part   : String
        := Enquote_If_Needed (S (Header.Local_Part),
                              Requires_Quoting => not (RFC2822_Atom_Text or
                                                       To_Set (".")),
                              Add_Quotation    => False);
      Domain_Part  : String := Enquote_Domain (S (Header.Domain_Part));
      Display_Name : String := Format_Name (Name_Value);

   begin -- Get_Message_ID

      return Display_Name & "<" & Local_Part & "@" & Domain_Part & ">";

   end Get_Full_Address;

end Basil.Address_Headers;
