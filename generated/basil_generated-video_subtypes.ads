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
--  <http://www.iana.org/assignments/media-types/video/>
--  which is Copyright (C) The Internet Corporation for Assigned Names
--  and Numbers All rights reserved.
--
--  Generated: Wed, 16 Jul 2008 18:40 CDT by jordanb@murat
--
-------------------------------------------------------------------------------

package Basil_Generated.Video_Subtypes is

   pragma Pure;

   type Video_Content_Subtype is
     (ST_Unknown,
      ST_Experimental,
      ST_3gpp,
      ST_3gpp2,
      ST_3gpp_Tt,
      ST_BMPEG,
      ST_BT656,
      ST_CelB,
      ST_DV,
      ST_Example,
      ST_H261,
      ST_H263,
      ST_H263_1998,
      ST_H263_2000,
      ST_H264,
      ST_JPEG,
      ST_MJ2,
      ST_MP1S,
      ST_MP2P,
      ST_MP2T,
      ST_Mp4,
      ST_MP4V_ES,
      ST_MPV,
      ST_Mpeg,
      ST_Mpeg4_Generic,
      ST_Nv,
      ST_Parityfec,
      ST_Pointer,
      ST_Quicktime,
      ST_Raw,
      ST_Rtp_Enc_Aescm128,
      ST_Rtx,
      ST_SMPTE292M,
      ST_Ulpfec,
      ST_Vc1,
      ST_Vnd_D_Dlna_D_Mpeg_Tts,
      ST_Vnd_D_Fvt,
      ST_Vnd_D_Hns_D_Video,
      ST_Vnd_D_Iptvforum_D_1dparityfec_1010,
      ST_Vnd_D_Iptvforum_D_1dparityfec_2005,
      ST_Vnd_D_Iptvforum_D_2dparityfec_1010,
      ST_Vnd_D_Iptvforum_D_2dparityfec_2005,
      ST_Vnd_D_Iptvforum_D_Ttsavc,
      ST_Vnd_D_Iptvforum_D_Ttsmpeg2,
      ST_Vnd_D_Motorola_D_Video,
      ST_Vnd_D_Motorola_D_Videop,
      ST_Vnd_D_Mpegurl,
      ST_Vnd_D_Ms_Playready_D_Media_D_Pyv,
      ST_Vnd_D_Nokia_D_Interleaved_Multimedia,
      ST_Vnd_D_Nokia_D_Videovoip,
      ST_Vnd_D_Objectvideo,
      ST_Vnd_D_Sealed_D_Mpeg1,
      ST_Vnd_D_Sealed_D_Mpeg4,
      ST_Vnd_D_Sealed_D_Swf,
      ST_Vnd_D_Sealedmedia_D_Softseal_D_Mov,
      ST_Vnd_D_Vivo);

end Basil_Generated.Video_Subtypes;
