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

-------------------------------------------------------------------------------
-- EXTERNAL DOCUMENTATION:
--
--  MRCY.SPC.002
--
-------------------------------------------------------------------------------
-- PROJECT CONTEXT:
--
--  This package provides the Internet Message object, and operations
--  to manage it. Please note that an internet message is a derivitive
--  of the Abstract_Entity defined in basil-entities.ads.
--
-------------------------------------------------------------------------------

with Basil.Strings;                  use Basil.Strings;
with Basil.Address_Headers;          use Basil.Address_Headers;
with Basil.Date_Headers;             use Basil.Date_Headers;
with Basil.Address_Headers.Lists;    use Basil.Address_Headers.Lists;

package Basil.Entities.Messages is

   --  Operations on child MIME entities of messages are ineritied
   --  from the entity object. Please see basil-entities.ads.
   type Message is new Basil.Entities.Abstract_Entity with private;

   ----------------------------------------------------------------------------
   --  This function takes an object representation of an message and
   --  returns a serialized string representation. This function does
   --  some validation of the object.
   --
   --  If the content type headers do not match the private content
   --  type object, and the object type is not Unknown or Experimental
   --  it will raise Invalid_Content_Type. If the content type has
   --  Experimental components (either type or subtype) the function
   --  will ensure that the header is parsable and that the type label
   --  is formed properly. If the type is Unknown then no validation
   --  will be done.
   function To_String (Source : in Message)
                      return String;

   ----------------------------------------------------------------------------
   --  This function takes a string containing a serialized message at
   --  the index Start and returns an object representation. If the
   --  message has children the function will recursivly unserialize
   --  each of them into data structures.
   function From_String (Source : in String;
                         Start  : in Positive := 1)
                        return Message;

   ----------------------------------------------------------------------------
   --  This version of From String provides more information about the
   --  string than above. It is useful when the message may not be the
   --  last message in the string, and sets Index to reference the
   --  first character after the last character of the message.
   procedure From_String (New_Message :    out Message;
                          Source      : in     String;
                          Index       : in out Positive);

   ----------------------------------------------------------------------------
   --  This function constructs a message object based on the
   --  inputs. The To and From lists must have at least one address
   --  header object. The Subject and Body_Part may be an empty
   --  string.
   --
   --  By default the content type of the message is text/plain with a
   --  CTE 7bit. You may change this after creation of the message. Be
   --  warned that if you change it to Multipart the body part may not
   --  be used the way you expect. Please see basil-entities.ads for
   --  more information.
   function Compose        (From      : in Address_Headers.Lists.List;
                            To        : in Address_Headers.Lists.List;
                            Subject   : in Encoded_String;
                            Body_Part : in Encoded_String)
                           return Message;

   ----------------------------------------------------------------------------
   --  This version of Compose is a convenience function to cover the
   --  common case where there is only one address in the "To" field
   --  and one in the "From" field. It prevents you having to create a
   --  one-element list of address headers for each.
   --
   --  By default the content type of the message is text/plain with a
   --  CTE 7bit. You may change this after creation of the message. Be
   --  warned that if you change it to Multipart the body part may not
   --  be used the way you expect. Please see basil-entities.ads for
   --  more information.
   function Compose        (From      : in Address_Header;
                            To        : in Address_Header;
                            Subject   : in Encoded_String;
                            Body_Part : in Encoded_String)
                           return Message;

   ----------------------------------------------------------------------------
   --  This version of Compose is provided in the interest of being
   --  helpful but its use is discouraged. It does NO validation of
   --  either the From or To structured addres header.
   function Compose        (From      : in Encoded_String;
                            To        : in Encoded_String;
                            Subject   : in Encoded_String;
                            Body_Part : in Encoded_String)
                           return Message;

   ----------------------------------------------------------------------------
   --  Content type accessor functions. Set_Content_Type updates the
   --  headers list to reflect the new content type unless it has
   --  Experimental or Unknown components (in which case you're on
   --  your own managing the content type headers). This version of
   --  these functions override those for entities to add MIME version
   --  headers.
   --
   --  PLEASE NOTE: According to RFC 2045 p 16 entities of type
   --  multipart may not have a content transfer encoding of
   --  quoted-printable or base64. If Set_Content_Type is used to
   --  apply a Content Type with those properties it will raise
   --  Invalid_Content_Type.
   overriding procedure Set_Content_Type (Entity : in out Message;
                                          Value  : in     Content_Type);

   ----------------------------------------------------------------------------
   --  This function returns a list of address headers culled from
   --  every header that may contain a sender address according to RFC
   --  2822 (Sender, From, etc).
   function Get_Senders         (Item : in Message)
                                return Address_Headers.Lists.List;

   ----------------------------------------------------------------------------
   --  This function returns a list of address headers culled from
   --  every header that may contain a recipient address according to
   --  RFC 2822 (To, Cc, etc).
   function Get_Recipients      (Item : in Message)
                                return Address_Headers.Lists.List;

   ----------------------------------------------------------------------------
   --  This function returns a Date_Header object containing the value
   --  of the "Orig-Date" header of the message.
   function Get_Creation_Date   (Item : in Message)
                                return Date_Headers.Date_Header;

   ----------------------------------------------------------------------------
   --  This procedure sets the "Orig-Date" header to represent the
   --  time in the Value date header object.
   procedure Set_Creation_Date (Item  : in out Message;
                                Value : in     Date_Header);


private

   type Message is new Abstract_Entity with null record;

end Basil.Entities.Messages;
