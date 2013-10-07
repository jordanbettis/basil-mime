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

with Ada.Calendar;                  use Ada.Calendar;
with Ada.Containers;                use Ada.Containers;

with Basil.Utils;                   use Basil.Utils;
with Basil.Entities.Serialization;  use Basil.Entities.Serialization;
with Basil.Message_ID_Headers;      use Basil.Message_ID_Headers;

with Basil.Entities.MIME;           use  Basil.Entities.MIME;

package body Basil.Entities.Messages is

   ----------------------------------------------------------------------------
   --  Given a message and a key, this function will find the header
   --  in the message with the key and parse the value to return a
   --  list of addresses.
   function Get_Addresses     (Item : in Message;
                               Key  : in Encoded_String)
                                return Address_Headers.Lists.List;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function To_String (Source : in Message)
                      return String is

   begin -- To_String

      return Serialize_Entity (Source);

   end To_String;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function From_String (Source : in String;
                         Start  : in Positive := 1)
                        return Message is

      Index       : Positive := Start;
      New_Message : Message;

   begin -- From_String

      From_String (New_Message => New_Message,
                   Source      => Source,
                   Index       => Index);

      return New_Message;

   end From_String;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure From_String (New_Message :    out Message;
                          Source      : in     String;
                          Index       : in out Positive) is

   begin -- From_String

      Unserialize_Entity (Source       => Source,
                          Delimiter    => "",
                          Index        => Index,
                          Entity       => New_Message,
                          Get_Children => Get_Children'Access);

   end From_String;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Compose        (From      : in Address_Headers.Lists.List;
                            To        : in Address_Headers.Lists.List;
                            Subject   : in Encoded_String;
                            Body_Part : in Encoded_String)
                           return Message is

      From_List : Headers.Lists.List := Compile (From);
      To_List   : Headers.Lists.List := Compile (To);

   begin -- Compose

      if Headers.Lists.Length (From_List) /= 1
        or Headers.Lists.Length (To_List) /= 1
      then
         raise Invalid_Header;
      end if;

      return
        Compose (From      => ES (Headers.Get_Value
                                  (Headers.Lists.First_Element (From_List))),
                 To        => ES (Headers.Get_Value
                                  (Headers.Lists.First_Element (To_List))),
                 Subject   => Subject,
                 Body_Part => Body_Part);

   end Compose;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Compose        (From      : in Address_Headers.Address_Header;
                            To        : in Address_Headers.Address_Header;
                            Subject   : in Encoded_String;
                            Body_Part : in Encoded_String)
                           return Message is

      From_Addresses : Address_Headers.Lists.List;
      To_Addresses   : Address_Headers.Lists.List;

   begin -- Compose

      Append (From_Addresses, From);
      Append (To_Addresses, To);

      return
        Compose (From    => From_Addresses,
                 To      => To_Addresses,
                 Subject => Subject,
                 Body_Part => Body_Part);

   end Compose;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Compose        (From      : in Encoded_String;
                            To        : in Encoded_String;
                            Subject   : in Encoded_String;
                            Body_Part : in Encoded_String)
                           return Message is

      New_Message : Message;

   begin -- Compose

      Headers.Lists.Append (New_Message.Headers_List,
                            Headers.To_Header ("From", From));

      Headers.Lists.Append (New_Message.Headers_List,
                            Headers.To_Header ("To", From));

      Headers.Lists.Append (New_Message.Headers_List,
                            Date_Headers.Compile
                            (To_Date_Header ("Date", Clock)));

      Headers.Lists.Append (New_Message.Headers_List,
                            Headers.To_Header ("Message-ID",
                                               ES (Get_Message_ID
                                                   (Generate_Message_ID
                                                    ("Message-ID")))));

      Headers.Lists.Append (New_Message.Headers_List,
                            Headers.To_Header ("Subject", Subject));

      Set_Content_Type (New_Message,
                        Content_Type'(MIME_Type     => T_Text,
                                      Text_Subtype  => ST_Plain,
                                      Character_Set => CS_US_ASCII,
                                      Content_Transfer_Encoding =>
                                        CTE_7bit));

      New_Message.Body_Part := EUS (Body_Part);

      return New_Message;

   end Compose;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   overriding procedure Set_Content_Type (Entity : in out Message;
                                          Value  : in     Content_Type) is

      -------------------------------------------------------------------------
      --  Ensures that there are no experimental or unknown components
      --  that prevent serialization of the content type object. If
      --  there are, it's up to the user to make sure there are valid
      --  content type headers in the entity.
      function Is_Serializable (Value : in Content_Type) return Boolean;

      function Is_Serializable (Value : in Content_Type) return Boolean is

      begin -- Is_Serializable

         if Value.Content_Transfer_Encoding = CTE_Unknown or
           Value.Content_Transfer_Encoding = CTE_Experimental
         then
            return False;
         end if;

         if Get_Content_Class (Value) /= Normal then
            return False;
         end if;

         return True;

      end Is_Serializable;

      -------------------------------------------------------------------------
      Type_Headers   : Headers.Lists.List;

   begin -- Set_Content_Type

      if Value.MIME_Type = T_Multipart and
        (Value.Content_Transfer_Encoding = CTE_Quoted_Printable or
         Value.Content_Transfer_Encoding = CTE_Base64)
      then
         raise Invalid_Content_Type;
      end if;

      Entity.Entity_Type := Value;

      if Is_Serializable (Value) then

         Type_Headers := To_Headers (Source      => Value,
                                     Add_Version => True);

         Headers.Lists.Purge_Header (Entity.Headers_List, "MIME-VERSION");
         Headers.Lists.Purge_Header (Entity.Headers_List, "CONTENT-TYPE");
         Headers.Lists.Purge_Header
           (Entity.Headers_List, "CONTENT-TRANSFER-ENCODING");

         Headers.Lists.Splice (Target => Entity.Headers_List,
                               Before => Headers.Lists.No_Element,
                               Source => Type_Headers);

      end if;

   end Set_Content_Type;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_Senders         (Item : in Message)
                                return Address_Headers.Lists.List is

      Address_List : Address_Headers.Lists.List;
      New_List     : Address_Headers.Lists.List;

   begin -- Get_Recipients

      New_List := Get_Addresses (Item, "FROM");
      Splice (Target => Address_List,
              Before => Address_Headers.Lists.No_Element,
              Source => New_List);

      New_List := Get_Addresses (Item, "SENDER");
      Splice (Target => Address_List,
              Before => Address_Headers.Lists.No_Element,
              Source => New_List);

      New_List := Get_Addresses (Item, "REPLY-TO");
      Splice (Target => Address_List,
              Before => Address_Headers.Lists.No_Element,
              Source => New_List);

      New_List := Get_Addresses (Item, "RESENT-FROM");
      Splice (Target => Address_List,
              Before => Address_Headers.Lists.No_Element,
              Source => New_List);

      New_List := Get_Addresses (Item, "RESENT-SENDER");
      Splice (Target => Address_List,
              Before => Address_Headers.Lists.No_Element,
              Source => New_List);

      return Address_List;

   end Get_Senders;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_Recipients      (Item : in Message)
                                return Address_Headers.Lists.List is

      Address_List : Address_Headers.Lists.List;
      New_List     : Address_Headers.Lists.List;

   begin -- Get_Recipients

      New_List := Get_Addresses (Item, "TO");
      Splice (Target => Address_List,
              Before => Address_Headers.Lists.No_Element,
              Source => New_List);

      New_List := Get_Addresses (Item, "CC");
      Splice (Target => Address_List,
              Before => Address_Headers.Lists.No_Element,
              Source => New_List);

      New_List := Get_Addresses (Item, "BCC");
      Splice (Target => Address_List,
              Before => Address_Headers.Lists.No_Element,
              Source => New_List);

      New_List := Get_Addresses (Item, "RESENT-TO");
      Splice (Target => Address_List,
              Before => Address_Headers.Lists.No_Element,
              Source => New_List);

      New_List := Get_Addresses (Item, "RESENT-CC");
      Splice (Target => Address_List,
              Before => Address_Headers.Lists.No_Element,
              Source => New_List);

      New_List := Get_Addresses (Item, "RESENT-BCC");
      Splice (Target => Address_List,
              Before => Address_Headers.Lists.No_Element,
              Source => New_List);

      New_List := Get_Addresses (Item, "DELIVERED-TO");
      Splice (Target => Address_List,
              Before => Address_Headers.Lists.No_Element,
              Source => New_List);

      return Address_List;

   end Get_Recipients;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_Creation_Date   (Item : in Message)
                                return Date_Headers.Date_Header is

      Header_Cursor : Headers.Lists.Cursor;

   begin -- Get_Creation_Date

      Header_Cursor := Headers.Lists.Find (Item.Headers_List,
                                           Headers.To_Header ("DATE", ""));

      if Headers.Lists.Has_Element (Header_Cursor) then
         return Date_Headers.Parse (Headers.Lists.Element (Header_Cursor));
      else
         raise Invalid_Message_Or_Entity;
      end if;

   end Get_Creation_Date;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Set_Creation_Date (Item  : in out Message;
                                Value : in     Date_Headers.Date_Header) is

   begin -- Set_Creation_date

      Headers.Lists.Purge_Header (Header_List => Item.Headers_List,
                                  Key         => "DATE");

      Headers.Lists.Ensure_Header_Exists
        (Header_List   => Item.Headers_List,
         Header_Object => Date_Headers.Compile (Value));

   end Set_Creation_Date;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   function Get_Addresses     (Item : in Message;
                               Key  : in Encoded_String)
                                return Address_Headers.Lists.List is

      Header_Cursor : Headers.Lists.Cursor;

   begin -- Get_Recipients

      Header_Cursor := Headers.Lists.Find (Item.Headers_List,
                                           Headers.To_Header (Key, ""));

      if Headers.Lists.Has_Element (Header_Cursor) then
         return Parse (Headers.Lists.Element (Header_Cursor));
      else
         return Address_Headers.Lists.Empty_List;
      end if;

   end Get_Addresses;

end Basil.Entities.Messages;
