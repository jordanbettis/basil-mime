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
-- PROJECT CONTEXT:
--
--  This is an auto-generated file which contains the enumeration
--  subtype definition for the named content subtype. It is renamed
--  into basil.ads so that the enumeration is avaliable throughout the
--  library.
--
-------------------------------------------------------------------------------
-- NOTICE:
--
--  This file was generated automatically from the database at
--  <http://www.iana.org/assignments/media-types/multipart/>
--  which is Copyright (C) The Internet Corporation for Assigned Names
--  and Numbers All rights reserved.
--
--  Generated: Wed, 16 Jul 2008 18:40 CDT by jordanb@murat
--
-------------------------------------------------------------------------------

package Basil_Generated.Multipart_Subtypes is

   pragma Pure;

   type Multipart_Content_Subtype is
     (ST_Unknown,
      ST_Experimental,
      ST_Alternative,
      ST_Appledouble,
      ST_Byteranges,
      ST_Digest,
      ST_Encrypted,
      ST_Example,
      ST_Form_Data,
      ST_Header_Set,
      ST_Mixed,
      ST_Parallel,
      ST_Related,
      ST_Report,
      ST_Signed,
      ST_Voice_Message);

end Basil_Generated.Multipart_Subtypes;
