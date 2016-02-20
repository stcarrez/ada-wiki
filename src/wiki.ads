-----------------------------------------------------------------------
--  wiki -- Ada Wiki Engine
--  Copyright (C) 2015, 2016 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------

package Wiki is

   pragma Preelaborate;

   type Format_Type is (BOLD, ITALIC, CODE, SUPERSCRIPT, SUBSCRIPT, STRIKEOUT, PREFORMAT);

   type Format_Map is array (Format_Type) of Boolean;

   --  The possible HTML tags as described in HTML5 specification.
   type Html_Tag is
     (
      --  Section 4.1 The root element
      ROOT_HTML_TAG,

      --  Section 4.2 Document metadata
      HEAD_TAG, TITLE_TAG, BASE_TAG, LINK_TAG, META_TAG, STYLE_TAG,

      --  Section 4.3 Sections
      BODY_TAG, ARTICLE_TAG, SECTION_TAG, NAV_TAG, ASIDE_TAG,
      H1_TAG, H2_TAG, H3_TAG, H4_TAG, H5_TAG, H6_TAG,
      HEADER_TAG, FOOTER_TAG,
      ADDRESS_TAG,

      --  Section 4.4 Grouping content
      P_TAG, HR_TAG, PRE_TAG, BLOCKQUOTE_TAG,
      OL_TAG, UL_TAG, LI_TAG,
      DL_TAG, DT_TAG, DD_TAG,
      FIGURE_TAG, FIGCAPTION_TAG,
      DIV_TAG, MAIN_TAG,

      --  Section 4.5 Text-level semantics
      A_TAG, EM_TAG, STRONG_TAG, SMALL_TAG,
      S_TAG, CITE_TAG, Q_TAG, DFN_TAG, ABBR_TAG,
      DATA_TAG, TIME_TAG, CODE_TAG, VAR_TAG, SAMP_TAG,
      KBD_TAG, SUB_TAG, SUP_TAG,
      I_TAG, B_TAG, U_TAG,
      MARK_TAG, RUBY_TAG, RB_TAG, RT_TAG, RTC_TAG,
      RP_TAG, BDI_TAG, BDO_TAG, SPAN_TAG,
      BR_TAG, WBR_TAG,

      --  Section 4.6 Edits
      INS_TAG, DEL_TAG,

      --  Section 4.7 Embedded content
      IMG_TAG,
      IFRAME_TAG,
      EMBED_TAG,
      OBJECT_TAG,
      PARAM_TAG,
      VIDEO_TAG,
      AUDIO_TAG,
      SOURCE_TAG,
      TRACK_TAG,
      MAP_TAG,
      AREA_TAG,

      --  Section 4.9 Tabular data
      TABLE_TAG, CAPTION_TAG, COLGROUP_TAG, COL_TAG,
      TBODY_TAG, THEAD_TAG, TFOOT_TAG,
      TR_TAG, TD_TAG, TH_TAG,

      --  Section 4.10 Forms
      FORM_TAG, LABEL_TAG, INPUT_TAG,
      BUTTON_TAG, SELECT_TAG, DATALIST_TAG, OPTGROUP_TAG,
      OPTION_TAG, TEXTAREA_TAG, KEYGEN_TAG, OUTPUT_TAG,
      PROGRESS_TAG, METER_TAG, FIELDSET_TAG, LEGEND_TAG,

      --  Section 4.11 Scripting
      SCRIPT_TAG, NOSCRIPT_TAG,
      TEMPLATE_TAG, CANVAS_TAG,

      --  Unknown tags
      UNKNOWN_TAG
     );

   --  Find the tag from the tag name.
   function Find_Tag (Name : in Wide_Wide_String) return Html_Tag;

   type String_Access is access constant String;

   --  Get the HTML tag name.
   function Get_Tag_Name (Tag : in Html_Tag) return String_Access;

end Wiki;
