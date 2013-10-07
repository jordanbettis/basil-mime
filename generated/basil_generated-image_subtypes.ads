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
--  <http://www.iana.org/assignments/media-types/image/>
--  which is Copyright (C) The Internet Corporation for Assigned Names
--  and Numbers All rights reserved.
--
--  Generated: Wed, 16 Jul 2008 18:40 CDT by jordanb@murat
--
-------------------------------------------------------------------------------

package Basil_Generated.Image_Subtypes is

   pragma Pure;

   type Image_Content_Subtype is
     (ST_Unknown,
      ST_Experimental,
      ST_Cgm,
      ST_Example,
      ST_Fits,
      ST_G3fax,
      ST_Gif,
      ST_Ief,
      ST_Jp2,
      ST_Jpeg,
      ST_Jpm,
      ST_Jpx,
      ST_Naplps,
      ST_Png,
      ST_Prs_D_Btif,
      ST_Prs_D_Pti,
      ST_T38,
      ST_Tiff,
      ST_Tiff_Fx,
      ST_Vnd_D_Adobe_D_Photoshop,
      ST_Vnd_D_Cns_D_Inf2,
      ST_Vnd_D_Djvu,
      ST_Vnd_D_Dwg,
      ST_Vnd_D_Dxf,
      ST_Vnd_D_Fastbidsheet,
      ST_Vnd_D_Fpx,
      ST_Vnd_D_Fst,
      ST_Vnd_D_Fujixerox_D_Edmics_Mmr,
      ST_Vnd_D_Fujixerox_D_Edmics_Rlc,
      ST_Vnd_D_Globalgraphics_D_Pgb,
      ST_Vnd_D_Microsoft_D_Icon,
      ST_Vnd_D_Mix,
      ST_Vnd_D_Ms_Modi,
      ST_Vnd_D_Net_Fpx,
      ST_Vnd_D_Sealed_D_Png,
      ST_Vnd_D_Sealedmedia_D_Softseal_D_Gif,
      ST_Vnd_D_Sealedmedia_D_Softseal_D_Jpg,
      ST_Vnd_D_Svf,
      ST_Vnd_D_Wap_D_Wbmp,
      ST_Vnd_D_Xiff);

end Basil_Generated.Image_Subtypes;
