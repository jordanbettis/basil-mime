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
--  <http://www.iana.org/assignments/media-types/audio/>
--  which is Copyright (C) The Internet Corporation for Assigned Names
--  and Numbers All rights reserved.
--
--  Generated: Wed, 16 Jul 2008 18:40 CDT by jordanb@murat
--
-------------------------------------------------------------------------------

package Basil_Generated.Audio_Subtypes is

   pragma Pure;

   type Audio_Content_Subtype is
     (ST_Unknown,
      ST_Experimental,
      ST_32kadpcm,
      ST_3gpp,
      ST_3gpp2,
      ST_Ac3,
      ST_AMR,
      ST_AMR_WB,
      ST_Amr_Wb_P,
      ST_Asc,
      ST_Basic,
      ST_BV16,
      ST_BV32,
      ST_Clearmode,
      ST_CN,
      ST_DAT12,
      ST_Dls,
      ST_Dsr_Es201108,
      ST_Dsr_Es202050,
      ST_Dsr_Es202211,
      ST_Dsr_Es202212,
      ST_Eac3,
      ST_DVI4,
      ST_EVRC,
      ST_EVRC0,
      ST_EVRC1,
      ST_EVRCB,
      ST_EVRCB0,
      ST_EVRCB1,
      ST_EVRC_QCP,
      ST_EVRCWB,
      ST_EVRCWB0,
      ST_EVRCWB1,
      ST_Example,
      ST_G722,
      ST_G7221,
      ST_G723,
      ST_G726_16,
      ST_G726_24,
      ST_G726_32,
      ST_G726_40,
      ST_G728,
      ST_G729,
      ST_G7291,
      ST_G729D,
      ST_G729E,
      ST_GSM,
      ST_GSM_EFR,
      ST_ILBC,
      ST_L8,
      ST_L16,
      ST_L20,
      ST_L24,
      ST_LPC,
      ST_Mobile_Xmf,
      ST_MPA,
      ST_Mp4,
      ST_MP4A_LATM,
      ST_Mpa_Robust,
      ST_Mpeg,
      ST_Mpeg4_Generic,
      ST_Parityfec,
      ST_PCMA,
      ST_PCMU,
      ST_Prs_D_Sid,
      ST_QCELP,
      ST_RED,
      ST_Rtp_Enc_Aescm128,
      ST_Rtp_Midi,
      ST_Rtx,
      ST_SMV,
      ST_SMV0,
      ST_SMV_QCP,
      ST_Sp_Midi,
      ST_T140c,
      ST_T38,
      ST_Telephone_Event,
      ST_Tone,
      ST_Ulpfec,
      ST_VDVI,
      ST_VMR_WB,
      ST_Vnd_D_3gpp_D_Iufp,
      ST_Vnd_D_4SB,
      ST_Vnd_D_Audiokoz,
      ST_Vnd_D_CELP,
      ST_Vnd_D_Cisco_D_Nse,
      ST_Vnd_D_Cmles_D_Radio_Events,
      ST_Vnd_D_Cns_D_Anp1,
      ST_Vnd_D_Cns_D_Inf1,
      ST_Vnd_D_Digital_Winds,
      ST_Vnd_D_Dlna_D_Adts,
      ST_Vnd_D_Dolby_D_Mlp,
      ST_Vnd_D_Everad_D_Plj,
      ST_Vnd_D_Hns_D_Audio,
      ST_Vnd_D_Lucent_D_Voice,
      ST_Vnd_D_Ms_Playready_D_Media_D_Pya,
      ST_Vnd_D_Nokia_D_Mobile_Xmf,
      ST_Vnd_D_Nortel_D_Vbk,
      ST_Vnd_D_Nuera_D_Ecelp4800,
      ST_Vnd_D_Nuera_D_Ecelp7470,
      ST_Vnd_D_Nuera_D_Ecelp9600,
      ST_Vnd_D_Octel_D_Sbc,
      ST_Vnd_D_Qcelp,
      ST_Vnd_D_Rhetorex_D_32kadpcm,
      ST_Vnd_D_Sealedmedia_D_Softseal_D_Mpeg,
      ST_Vnd_D_Vmx_D_Cvsd,
      ST_Vorbis,
      ST_Vorbis_Config);

end Basil_Generated.Audio_Subtypes;
