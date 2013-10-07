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
--  <http://www.iana.org/assignments/media-types/text/>
--  which is Copyright (C) The Internet Corporation for Assigned Names
--  and Numbers All rights reserved.
--
--  Generated: Wed, 16 Jul 2008 18:40 CDT by jordanb@murat
--
-------------------------------------------------------------------------------

package Basil_Generated.Text_Subtypes is

   pragma Pure;

   type Text_Content_Subtype is
     (ST_Unknown,
      ST_Experimental,
      ST_Calendar,
      ST_Css,
      ST_Csv,
      ST_Directory,
      ST_Dns,
      ST_Ecmascript,
      ST_Enriched,
      ST_Example,
      ST_Html,
      ST_Javascript,
      ST_Parityfec,
      ST_Plain,
      ST_Prs_D_Fallenstein_D_Rst,
      ST_Prs_D_Lines_D_Tag,
      ST_RED,
      ST_Rfc822_Headers,
      ST_Richtext,
      ST_Rtf,
      ST_Rtp_Enc_Aescm128,
      ST_Rtx,
      ST_Sgml,
      ST_T140,
      ST_Tab_Separated_Values,
      ST_Troff,
      ST_Ulpfec,
      ST_Uri_List,
      ST_Vnd_D_Abc,
      ST_Vnd_D_Curl,
      ST_Vnd_D_DMClientScript,
      ST_Vnd_D_Esmertec_D_Theme_Descriptor,
      ST_Vnd_D_Fly,
      ST_Vnd_D_Fmi_D_Flexstor,
      ST_Vnd_D_In3d_D_3dml,
      ST_Vnd_D_In3d_D_Spot,
      ST_Vnd_D_IPTC_D_NewsML,
      ST_Vnd_D_IPTC_D_NITF,
      ST_Vnd_D_Latex_Z,
      ST_Vnd_D_Motorola_D_Reflex,
      ST_Vnd_D_Ms_Mediapackage,
      ST_Vnd_D_Net2phone_D_Commcenter_D_Command,
      ST_Vnd_D_Si_D_Uricatalogue,
      ST_Vnd_D_Sun_D_J2me_D_App_Descriptor,
      ST_Vnd_D_Trolltech_D_Linguist,
      ST_Vnd_D_Wap_D_Si,
      ST_Vnd_D_Wap_D_Sl,
      ST_Vnd_D_Wap_D_Wml,
      ST_Vnd_D_Wap_D_Wmlscript,
      ST_Xml,
      ST_Xml_External_Parsed_Entity);

end Basil_Generated.Text_Subtypes;
