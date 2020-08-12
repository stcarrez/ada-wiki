-----------------------------------------------------------------------
--  wiki -- Ada Wiki Engine
--  Copyright (C) 2015, 2016, 2018, 2020 Stephane Carrez
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
with Ada.Wide_Wide_Characters.Handling;
package body Wiki is

   type Tag_Array is array (Html_Tag) of String_Access;

   HTML_TAG_NAME                : aliased constant String := "html";
   HEAD_TAG_NAME                : aliased constant String := "head";
   TITLE_TAG_NAME               : aliased constant String := "title";
   BASE_TAG_NAME                : aliased constant String := "base";
   LINK_TAG_NAME                : aliased constant String := "link";
   META_TAG_NAME                : aliased constant String := "meta";
   STYLE_TAG_NAME               : aliased constant String := "style";
   BODY_TAG_NAME                : aliased constant String := "body";
   ARTICLE_TAG_NAME             : aliased constant String := "article";
   SECTION_TAG_NAME             : aliased constant String := "section";
   NAV_TAG_NAME                 : aliased constant String := "nav";
   ASIDE_TAG_NAME               : aliased constant String := "aside";
   H1_TAG_NAME                  : aliased constant String := "h1";
   H2_TAG_NAME                  : aliased constant String := "h2";
   H3_TAG_NAME                  : aliased constant String := "h3";
   H4_TAG_NAME                  : aliased constant String := "h4";
   H5_TAG_NAME                  : aliased constant String := "h5";
   H6_TAG_NAME                  : aliased constant String := "h6";
   HEADER_TAG_NAME              : aliased constant String := "header";
   FOOTER_TAG_NAME              : aliased constant String := "footer";
   ADDRESS_TAG_NAME             : aliased constant String := "address";
   P_TAG_NAME                   : aliased constant String := "p";
   HR_TAG_NAME                  : aliased constant String := "hr";
   PRE_TAG_NAME                 : aliased constant String := "pre";
   BLOCKQUOTE_TAG_NAME          : aliased constant String := "blockquote";
   OL_TAG_NAME                  : aliased constant String := "ol";
   UL_TAG_NAME                  : aliased constant String := "ul";
   LI_TAG_NAME                  : aliased constant String := "li";
   DL_TAG_NAME                  : aliased constant String := "dl";
   DT_TAG_NAME                  : aliased constant String := "dt";
   DD_TAG_NAME                  : aliased constant String := "dd";
   FIGURE_TAG_NAME              : aliased constant String := "figure";
   FIGCAPTION_TAG_NAME          : aliased constant String := "figcaption";
   DIV_TAG_NAME                 : aliased constant String := "div";
   MAIN_TAG_NAME                : aliased constant String := "main";
   A_TAG_NAME                   : aliased constant String := "a";
   EM_TAG_NAME                  : aliased constant String := "em";
   STRONG_TAG_NAME              : aliased constant String := "strong";
   SMALL_TAG_NAME               : aliased constant String := "small";
   S_TAG_NAME                   : aliased constant String := "s";
   CITE_TAG_NAME                : aliased constant String := "cite";
   Q_TAG_NAME                   : aliased constant String := "q";
   DFN_TAG_NAME                 : aliased constant String := "dfn";
   ABBR_TAG_NAME                : aliased constant String := "abbr";
   DATA_TAG_NAME                : aliased constant String := "data";
   TIME_TAG_NAME                : aliased constant String := "time";
   CODE_TAG_NAME                : aliased constant String := "code";
   VAR_TAG_NAME                 : aliased constant String := "var";
   SAMP_TAG_NAME                : aliased constant String := "samp";
   KBD_TAG_NAME                 : aliased constant String := "kbd";
   SUB_TAG_NAME                 : aliased constant String := "sub";
   SUP_TAG_NAME                 : aliased constant String := "sup";
   I_TAG_NAME                   : aliased constant String := "i";
   B_TAG_NAME                   : aliased constant String := "b";
   U_TAG_NAME                   : aliased constant String := "u";
   MARK_TAG_NAME                : aliased constant String := "mark";
   RUBY_TAG_NAME                : aliased constant String := "ruby";
   RB_TAG_NAME                  : aliased constant String := "rb";
   RT_TAG_NAME                  : aliased constant String := "rt";
   RTC_TAG_NAME                 : aliased constant String := "rtc";
   RP_TAG_NAME                  : aliased constant String := "rp";
   BDI_TAG_NAME                 : aliased constant String := "bdi";
   BDO_TAG_NAME                 : aliased constant String := "bdo";
   SPAN_TAG_NAME                : aliased constant String := "span";
   BR_TAG_NAME                  : aliased constant String := "br";
   WBR_TAG_NAME                 : aliased constant String := "wbr";
   INS_TAG_NAME                 : aliased constant String := "ins";
   DEL_TAG_NAME                 : aliased constant String := "del";
   IMG_TAG_NAME                 : aliased constant String := "img";
   IFRAME_TAG_NAME              : aliased constant String := "iframe";
   EMBED_TAG_NAME               : aliased constant String := "embed";
   OBJECT_TAG_NAME              : aliased constant String := "object";
   PARAM_TAG_NAME               : aliased constant String := "param";
   VIDEO_TAG_NAME               : aliased constant String := "video";
   AUDIO_TAG_NAME               : aliased constant String := "audio";
   SOURCE_TAG_NAME              : aliased constant String := "source";
   TRACK_TAG_NAME               : aliased constant String := "track";
   MAP_TAG_NAME                 : aliased constant String := "map";
   AREA_TAG_NAME                : aliased constant String := "area";
   TABLE_TAG_NAME               : aliased constant String := "table";
   CAPTION_TAG_NAME             : aliased constant String := "caption";
   COLGROUP_TAG_NAME            : aliased constant String := "colgroup";
   COL_TAG_NAME                 : aliased constant String := "col";
   TBODY_TAG_NAME               : aliased constant String := "tbody";
   THEAD_TAG_NAME               : aliased constant String := "thead";
   TFOOT_TAG_NAME               : aliased constant String := "tfoot";
   TR_TAG_NAME                  : aliased constant String := "tr";
   TD_TAG_NAME                  : aliased constant String := "td";
   TH_TAG_NAME                  : aliased constant String := "th";
   FORM_TAG_NAME                : aliased constant String := "form";
   LABEL_TAG_NAME               : aliased constant String := "label";
   INPUT_TAG_NAME               : aliased constant String := "input";
   BUTTON_TAG_NAME              : aliased constant String := "button";
   SELECT_TAG_NAME              : aliased constant String := "select";
   DATALIST_TAG_NAME            : aliased constant String := "datalist";
   OPTGROUP_TAG_NAME            : aliased constant String := "optgroup";
   OPTION_TAG_NAME              : aliased constant String := "option";
   TEXTAREA_TAG_NAME            : aliased constant String := "textarea";
   KEYGEN_TAG_NAME              : aliased constant String := "keygen";
   OUTPUT_TAG_NAME              : aliased constant String := "output";
   PROGRESS_TAG_NAME            : aliased constant String := "progress";
   METER_TAG_NAME               : aliased constant String := "meter";
   FIELDSET_TAG_NAME            : aliased constant String := "fieldset";
   LEGEND_TAG_NAME              : aliased constant String := "legend";
   SCRIPT_TAG_NAME              : aliased constant String := "script";
   NOSCRIPT_TAG_NAME            : aliased constant String := "noscript";
   TEMPLATE_TAG_NAME            : aliased constant String := "template";
   CANVAS_TAG_NAME              : aliased constant String := "canvas";
   TT_TAG_NAME                  : aliased constant String := "tt";
   UNKNOWN_TAG_NAME             : aliased constant String := "unknown";

   Tag_Names : constant Tag_Array :=
     (
      ROOT_HTML_TAG             => HTML_TAG_NAME'Access,
      HEAD_TAG                  => HEAD_TAG_NAME'Access,
      TITLE_TAG                 => TITLE_TAG_NAME'Access,
      BASE_TAG                  => BASE_TAG_NAME'Access,
      LINK_TAG                  => LINK_TAG_NAME'Access,
      META_TAG                  => META_TAG_NAME'Access,
      STYLE_TAG                 => STYLE_TAG_NAME'Access,
      BODY_TAG                  => BODY_TAG_NAME'Access,
      ARTICLE_TAG               => ARTICLE_TAG_NAME'Access,
      SECTION_TAG               => SECTION_TAG_NAME'Access,
      NAV_TAG                   => NAV_TAG_NAME'Access,
      ASIDE_TAG                 => ASIDE_TAG_NAME'Access,
      H1_TAG                    => H1_TAG_NAME'Access,
      H2_TAG                    => H2_TAG_NAME'Access,
      H3_TAG                    => H3_TAG_NAME'Access,
      H4_TAG                    => H4_TAG_NAME'Access,
      H5_TAG                    => H5_TAG_NAME'Access,
      H6_TAG                    => H6_TAG_NAME'Access,
      HEADER_TAG                => HEADER_TAG_NAME'Access,
      FOOTER_TAG                => FOOTER_TAG_NAME'Access,
      ADDRESS_TAG               => ADDRESS_TAG_NAME'Access,
      P_TAG                     => P_TAG_NAME'Access,
      HR_TAG                    => HR_TAG_NAME'Access,
      PRE_TAG                   => PRE_TAG_NAME'Access,
      BLOCKQUOTE_TAG            => BLOCKQUOTE_TAG_NAME'Access,
      OL_TAG                    => OL_TAG_NAME'Access,
      UL_TAG                    => UL_TAG_NAME'Access,
      LI_TAG                    => LI_TAG_NAME'Access,
      DL_TAG                    => DL_TAG_NAME'Access,
      DT_TAG                    => DT_TAG_NAME'Access,
      DD_TAG                    => DD_TAG_NAME'Access,
      FIGURE_TAG                => FIGURE_TAG_NAME'Access,
      FIGCAPTION_TAG            => FIGCAPTION_TAG_NAME'Access,
      DIV_TAG                   => DIV_TAG_NAME'Access,
      MAIN_TAG                  => MAIN_TAG_NAME'Access,
      A_TAG                     => A_TAG_NAME'Access,
      EM_TAG                    => EM_TAG_NAME'Access,
      STRONG_TAG                => STRONG_TAG_NAME'Access,
      SMALL_TAG                 => SMALL_TAG_NAME'Access,
      S_TAG                     => S_TAG_NAME'Access,
      CITE_TAG                  => CITE_TAG_NAME'Access,
      Q_TAG                     => Q_TAG_NAME'Access,
      DFN_TAG                   => DFN_TAG_NAME'Access,
      ABBR_TAG                  => ABBR_TAG_NAME'Access,
      DATA_TAG                  => DATA_TAG_NAME'Access,
      TIME_TAG                  => TIME_TAG_NAME'Access,
      CODE_TAG                  => CODE_TAG_NAME'Access,
      VAR_TAG                   => VAR_TAG_NAME'Access,
      SAMP_TAG                  => SAMP_TAG_NAME'Access,
      KBD_TAG                   => KBD_TAG_NAME'Access,
      SUB_TAG                   => SUB_TAG_NAME'Access,
      SUP_TAG                   => SUP_TAG_NAME'Access,
      I_TAG                     => I_TAG_NAME'Access,
      B_TAG                     => B_TAG_NAME'Access,
      U_TAG                     => U_TAG_NAME'Access,
      MARK_TAG                  => MARK_TAG_NAME'Access,
      RUBY_TAG                  => RUBY_TAG_NAME'Access,
      RB_TAG                    => RB_TAG_NAME'Access,
      RT_TAG                    => RT_TAG_NAME'Access,
      RTC_TAG                   => RTC_TAG_NAME'Access,
      RP_TAG                    => RP_TAG_NAME'Access,
      BDI_TAG                   => BDI_TAG_NAME'Access,
      BDO_TAG                   => BDO_TAG_NAME'Access,
      SPAN_TAG                  => SPAN_TAG_NAME'Access,
      BR_TAG                    => BR_TAG_NAME'Access,
      WBR_TAG                   => WBR_TAG_NAME'Access,
      INS_TAG                   => INS_TAG_NAME'Access,
      DEL_TAG                   => DEL_TAG_NAME'Access,
      IMG_TAG                   => IMG_TAG_NAME'Access,
      IFRAME_TAG                => IFRAME_TAG_NAME'Access,
      EMBED_TAG                 => EMBED_TAG_NAME'Access,
      OBJECT_TAG                => OBJECT_TAG_NAME'Access,
      PARAM_TAG                 => PARAM_TAG_NAME'Access,
      VIDEO_TAG                 => VIDEO_TAG_NAME'Access,
      AUDIO_TAG                 => AUDIO_TAG_NAME'Access,
      SOURCE_TAG                => SOURCE_TAG_NAME'Access,
      TRACK_TAG                 => TRACK_TAG_NAME'Access,
      MAP_TAG                   => MAP_TAG_NAME'Access,
      AREA_TAG                  => AREA_TAG_NAME'Access,
      TABLE_TAG                 => TABLE_TAG_NAME'Access,
      CAPTION_TAG               => CAPTION_TAG_NAME'Access,
      COLGROUP_TAG              => COLGROUP_TAG_NAME'Access,
      COL_TAG                   => COL_TAG_NAME'Access,
      TBODY_TAG                 => TBODY_TAG_NAME'Access,
      THEAD_TAG                 => THEAD_TAG_NAME'Access,
      TFOOT_TAG                 => TFOOT_TAG_NAME'Access,
      TR_TAG                    => TR_TAG_NAME'Access,
      TD_TAG                    => TD_TAG_NAME'Access,
      TH_TAG                    => TH_TAG_NAME'Access,
      FORM_TAG                  => FORM_TAG_NAME'Access,
      LABEL_TAG                 => LABEL_TAG_NAME'Access,
      INPUT_TAG                 => INPUT_TAG_NAME'Access,
      BUTTON_TAG                => BUTTON_TAG_NAME'Access,
      SELECT_TAG                => SELECT_TAG_NAME'Access,
      DATALIST_TAG              => DATALIST_TAG_NAME'Access,
      OPTGROUP_TAG              => OPTGROUP_TAG_NAME'Access,
      OPTION_TAG                => OPTION_TAG_NAME'Access,
      TEXTAREA_TAG              => TEXTAREA_TAG_NAME'Access,
      KEYGEN_TAG                => KEYGEN_TAG_NAME'Access,
      OUTPUT_TAG                => OUTPUT_TAG_NAME'Access,
      PROGRESS_TAG              => PROGRESS_TAG_NAME'Access,
      METER_TAG                 => METER_TAG_NAME'Access,
      FIELDSET_TAG              => FIELDSET_TAG_NAME'Access,
      LEGEND_TAG                => LEGEND_TAG_NAME'Access,
      SCRIPT_TAG                => SCRIPT_TAG_NAME'Access,
      NOSCRIPT_TAG              => NOSCRIPT_TAG_NAME'Access,
      TEMPLATE_TAG              => TEMPLATE_TAG_NAME'Access,
      CANVAS_TAG                => CANVAS_TAG_NAME'Access,
      TT_TAG                    => TT_TAG_NAME'Access,
      UNKNOWN_TAG               => UNKNOWN_TAG_NAME'Access
     );

   --  ------------------------------
   --  Get the HTML tag name.
   --  ------------------------------
   function Get_Tag_Name (Tag : in Html_Tag) return String_Access is
   begin
      return Tag_Names (Tag);
   end Get_Tag_Name;

   --  ------------------------------
   --  Find the tag from the tag name.
   --  ------------------------------
   function Find_Tag (Name : in Wide_Wide_String) return Html_Tag is

      function Tag (Name   : in Wide_Wide_String;
                    Expect : in Wide_Wide_String;
                    Tag    : in Html_Tag) return Html_Tag;

      function Tag (Name   : in Wide_Wide_String;
                    Expect : in Wide_Wide_String;
                    Tag    : in Html_Tag) return Html_Tag is
      begin
         if Ada.Wide_Wide_Characters.Handling.To_Lower (Name) = Expect then
            return Tag;
         else
            return UNKNOWN_TAG;
         end if;
      end Tag;
   begin
      --  The list of possible tags is well known and will not change very often.
      --  The tag lookup is implemented to be efficient with 2 or 3 embedded cases that
      --  reduce the comparison to the minimum.  The result is a large case statement that
      --  becomes unreadable.
      case Name'Length is
         when 0 =>
            return UNKNOWN_TAG;

         when 1 =>
            case Name (Name'First) is
            when 'a' | 'A' =>
               return A_TAG;
            when 'b' | 'B' =>
               return B_TAG;
            when 'i' | 'I' =>
               return I_TAG;
            when 'p' | 'P' =>
               return P_TAG;
            when 'q' | 'Q' =>
               return Q_TAG;
            when 's' | 'S' =>
               return S_TAG;
            when 'u' | 'U' =>
               return U_TAG;
            when others =>
               return UNKNOWN_TAG;
            end case;

         when 2 =>
            case Name (Name'First) is
            when 'b' | 'B' =>
               case Name (Name'Last) is
                  when 'r' | 'R' =>
                     return BR_TAG;
                  when others =>
                     return UNKNOWN_TAG;
               end case;

            when 'd' | 'D' =>
               case Name (Name'Last) is
                  when 'l' | 'L' =>
                     return DL_TAG;
                  when 't' | 'T' =>
                     return DT_TAG;
                  when 'd' | 'D' =>
                     return DD_TAG;
                  when others =>
                     return UNKNOWN_TAG;
               end case;

            when 'e' | 'E' =>
               case Name (Name'Last) is
                  when 'm' | 'M' =>
                     return EM_TAG;
                  when others =>
                     return UNKNOWN_TAG;
               end case;

            when 'h' | 'H' =>
               case Name (Name'Last) is
                  when '1' =>
                     return H1_TAG;
                  when '2' =>
                     return H2_TAG;
                  when '3' =>
                     return H3_TAG;
                  when '4' =>
                     return H4_TAG;
                  when '5' =>
                     return H5_TAG;
                  when '6' =>
                     return H6_TAG;
                  when 'r' | 'R' =>
                     return HR_TAG;
                  when others =>
                     return UNKNOWN_TAG;
               end case;

            when 'l' | 'L' =>
               case Name (Name'Last) is
                  when 'i' | 'I' =>
                     return LI_TAG;
                  when others =>
                     return UNKNOWN_TAG;
               end case;

            when 'o' | 'O' =>
               case Name (Name'Last) is
                  when 'l' | 'L' =>
                     return OL_TAG;
                  when others =>
                     return UNKNOWN_TAG;
               end case;

            when 'r' | 'R' =>
               case Name (Name'Last) is
                  when 'b' | 'B' =>
                     return RB_TAG;
                  when 'p' | 'P' =>
                     return RP_TAG;
                  when 't' | 'T' =>
                     return RT_TAG;
                  when others =>
                     return UNKNOWN_TAG;
               end case;

            when 't' | 'T' =>
               case Name (Name'Last) is
                  when 'r' | 'R' =>
                     return TR_TAG;
                  when 'd' | 'D' =>
                     return TD_TAG;
                  when 'h' | 'H' =>
                     return TH_TAG;
                  when 't' | 'T' =>
                     return TT_TAG;
                  when others =>
                     return UNKNOWN_TAG;
               end case;

            when 'u' | 'U' =>
               case Name (Name'Last) is
                  when 'l' | 'L' =>
                     return UL_TAG;
                  when others =>
                     return UNKNOWN_TAG;
               end case;

            when others =>
               return UNKNOWN_TAG;

            end case;

         when 3 =>
            case Name (Name'First) is
            when 'b' | 'B' =>
               case Name (Name'Last) is
                  when 'i' | 'I' =>
                     return Tag (Name, "bdi", BDI_TAG);
                  when 'o' | 'O' =>
                     return Tag (Name, "bdo", BDO_TAG);
                  when others =>
                     return UNKNOWN_TAG;
               end case;

            when 'c' | 'C' =>
               return Tag (Name, "col", COL_TAG);

            when 'd' | 'D' =>
               case Name (Name'First + 1) is
                  when 'i' | 'I' =>
                     return Tag (Name, "div", DIV_TAG);
                  when 'f' | 'F' =>
                     return Tag (Name, "dfn", DFN_TAG);
                  when 'e' | 'E' =>
                     return Tag (Name, "del", DEL_TAG);
                  when others =>
                     return UNKNOWN_TAG;
               end case;

            when 'i' | 'I' =>
               case Name (Name'Last) is
                  when 'g' | 'G' =>
                     return Tag (Name, "img", IMG_TAG);
                  when 's' | 'S' =>
                     return Tag (Name, "ins", INS_TAG);
                  when others =>
                     return UNKNOWN_TAG;
               end case;

            when 'k' | 'K' =>
               return Tag (Name, "kbd", KBD_TAG);

            when 'm' | 'M' =>
               return Tag (Name, "map", MAP_TAG);

            when 'n' | 'N' =>
               return Tag (Name, "nav", NAV_TAG);

            when 'p' | 'P' =>
               return Tag (Name, "pre", PRE_TAG);

            when 'r' | 'R' =>
               return Tag (Name, "rtc", RTC_TAG);

            when 's' | 'S' =>
               case Name (Name'Last) is
                  when 'b' | 'B' =>
                     return Tag (Name, "sub", SUB_TAG);
                  when 'p' | 'P' =>
                     return Tag (Name, "sup", SUP_TAG);
                  when others =>
                     return UNKNOWN_TAG;
               end case;

            when 'v' | 'V' =>
               return Tag (Name, "var", VAR_TAG);

            when 'w' | 'W' =>
               return Tag (Name, "wbr", WBR_TAG);

            when others =>
               return UNKNOWN_TAG;
            end case;

         when 4 =>
            case Name (Name'First) is
            when 'a' | 'A' =>
               case Name (Name'First + 1) is
                  when 'b' | 'B' =>
                     return Tag (Name, "abbr", ABBR_TAG);
                  when 'r' | 'R' =>
                     return Tag (Name, "area", AREA_TAG);
                  when others =>
                     return UNKNOWN_TAG;
               end case;

            when 'b' | 'B' =>
               case Name (Name'First + 1) is
                  when 'a' | 'A' =>
                     return Tag (Name, "base", BASE_TAG);
                  when 'o' | 'O' =>
                     return Tag (Name, "body", BODY_TAG);
                  when others =>
                     return UNKNOWN_TAG;
               end case;

            when 'c' | 'C' =>
               case Name (Name'First + 1) is
                  when 'i' | 'I' =>
                     return Tag (Name, "cite", CITE_TAG);
                  when 'o' | 'O' =>
                     return Tag (Name, "code", CODE_TAG);
                  when others =>
                     return UNKNOWN_TAG;
               end case;

            when 'd' | 'D' =>
               return Tag (Name, "data", DATA_TAG);

            when 'f' | 'F' =>
               return Tag (Name, "form", FORM_TAG);

            when 'h' | 'H' =>
               case Name (Name'First + 1) is
                  when 't' | 'T' =>
                     return Tag (Name, "html", ROOT_HTML_TAG);
                  when 'e' | 'E' =>
                     return Tag (Name, "head", HEAD_TAG);
                  when others =>
                     return UNKNOWN_TAG;
               end case;

            when 'l' | 'L' =>
               return Tag (Name, "link", LINK_TAG);

            when 'm' | 'M' =>
               case Name (Name'Last) is
                  when 'a' | 'A' =>
                     return Tag (Name, "meta", META_TAG);
                  when 'n' | 'N' =>
                     return Tag (Name, "main", MAIN_TAG);
                  when 'k' | 'K' =>
                     return Tag (Name, "mark", MARK_TAG);
                  when others =>
                     return UNKNOWN_TAG;
               end case;

            when 'r' | 'R' =>
               return Tag (Name, "ruby", RUBY_TAG);

            when 's' | 'S' =>
               case Name (Name'First + 1) is
                  when 'p' | 'P' =>
                     return Tag (Name, "span", SPAN_TAG);
                  when 'a' | 'A' =>
                     return Tag (Name, "samp", SAMP_TAG);
                  when others =>
                     return UNKNOWN_TAG;
               end case;

            when 't' | 'T' =>
               return Tag (Name, "time", TIME_TAG);

            when others =>
               return UNKNOWN_TAG;
            end case;

         when 5 =>
            case Name (Name'First) is
            when 'a' | 'A' =>
               case Name (Name'First + 1) is
                  when 's' | 'S' =>
                     return Tag (Name, "aside", ASIDE_TAG);
                  when 'u' | 'U' =>
                     return Tag (Name, "audio", AUDIO_TAG);
                  when others =>
                     return UNKNOWN_TAG;
               end case;

            when 'e' | 'E' =>
               return Tag (Name, "embed", EMBED_TAG);

            when 'i' | 'I' =>
               return Tag (Name, "input", INPUT_TAG);

            when 'l' | 'L' =>
               return Tag (Name, "label", LABEL_TAG);

            when 'm' | 'M' =>
               return Tag (Name, "meter", METER_TAG);

            when 'p' | 'P' =>
               return Tag (Name, "param", PARAM_TAG);

            when 's' | 'S' =>
               case Name (Name'First + 1) is
                  when 't' | 'T' =>
                     return Tag (Name, "style", STYLE_TAG);
                  when 'm' | 'M' =>
                     return Tag (Name, "small", SMALL_TAG);
                  when others =>
                     return UNKNOWN_TAG;
               end case;

            when 't' | 'T' =>
               case Name (Name'First + 1) is
                  when 'i' | 'I' =>
                     return Tag (Name, "title", TITLE_TAG);
                  when 'r' | 'R' =>
                     return Tag (Name, "track", TRACK_TAG);
                  when 'a' | 'A' =>
                     return Tag (Name, "table", TABLE_TAG);
                  when 'b' | 'B' =>
                     return Tag (Name, "tbody", TBODY_TAG);
                  when 'h' | 'H' =>
                     return Tag (Name, "thead", THEAD_TAG);
                  when 'f' | 'F' =>
                     return Tag (Name, "tfoot", TFOOT_TAG);
                  when others =>
                     return UNKNOWN_TAG;
               end case;

            when 'v' | 'V' =>
               return Tag (Name, "video", VIDEO_TAG);

            when others =>
               return UNKNOWN_TAG;
            end case;

         when others =>
            case Name (Name'First) is
            when 'a' | 'A' =>
               case Name (Name'First + 1) is
                  when 'r' | 'R' =>
                     return Tag (Name, "article", ARTICLE_TAG);
                  when 'd' | 'D' =>
                     return Tag (Name, "address", ADDRESS_TAG);
                  when others =>
                     return UNKNOWN_TAG;
               end case;

            when 'b' | 'B' =>
               case Name (Name'First + 1) is
                  when 'l' | 'L' =>
                     return Tag (Name, "blockquote", BLOCKQUOTE_TAG);
                  when 'u' | 'U' =>
                     return Tag (Name, "button", BUTTON_TAG);
                  when others =>
                     return UNKNOWN_TAG;
               end case;

            when 'c' | 'C' =>
               case Name (Name'First + 2) is
                  when 'p' | 'P' =>
                     return Tag (Name, "caption", CAPTION_TAG);
                  when 'l' | 'L' =>
                     return Tag (Name, "colgroup", COLGROUP_TAG);
                  when 'n' | 'N' =>
                     return Tag (Name, "canvas", CANVAS_TAG);
                  when others =>
                     return UNKNOWN_TAG;
               end case;

            when 'd' | 'D' =>
               return Tag (Name, "datalist", DATALIST_TAG);

            when 'f' | 'F' =>
               case Name (Name'Last) is
                  when 'r' | 'R' =>
                     return Tag (Name, "footer", FOOTER_TAG);
                  when 'e' | 'E' =>
                     return Tag (Name, "figure", FIGURE_TAG);
                  when 'n' | 'N' =>
                     return Tag (Name, "figcaption", FIGCAPTION_TAG);
                  when 't' | 'T' =>
                     return Tag (Name, "fieldset", FIELDSET_TAG);
                  when others =>
                     return UNKNOWN_TAG;
               end case;

            when 'h' | 'H' =>
               return Tag (Name, "header", HEADER_TAG);

            when 'i' | 'I' =>
               return Tag (Name, "iframe", IFRAME_TAG);

            when 'k' | 'K' =>
               return Tag (Name, "keygen", KEYGEN_TAG);

            when 'l' | 'L' =>
               return Tag (Name, "legend", LEGEND_TAG);

            when 'n' | 'N' =>
               return Tag (Name, "noscript", NOSCRIPT_TAG);

            when 'o' | 'O' =>
               case Name (Name'First + 3) is
                  when 'e' | 'E' =>
                     return Tag (Name, "object", OBJECT_TAG);
                  when 'g' | 'G' =>
                     return Tag (Name, "optgroup", OPTGROUP_TAG);
                  when 'i' | 'I' =>
                     return Tag (Name, "option", OPTION_TAG);
                  when 'p' | 'P' =>
                     return Tag (Name, "output", OUTPUT_TAG);
                  when others =>
                     return UNKNOWN_TAG;
               end case;

            when 'p' | 'P' =>
               return Tag (Name, "progress", PROGRESS_TAG);

            when 's' | 'S' =>
               case Name (Name'First + 3) is
                  when 't' | 'T' =>
                     return Tag (Name, "section", SECTION_TAG);
                  when 'o' | 'O' =>
                     return Tag (Name, "strong", STRONG_TAG);
                  when 'r' | 'R' =>
                     return Tag (Name, "source", SOURCE_TAG);
                  when 'e' | 'E' =>
                     return Tag (Name, "select", SELECT_TAG);
                  when 'i' | 'I' =>
                     return Tag (Name, "script", SCRIPT_TAG);
                  when others =>
                     return UNKNOWN_TAG;
               end case;

            when 't' | 'T' =>
               case Name (Name'Last) is
                  when 'a' | 'A' =>
                     return Tag (Name, "textarea", TEXTAREA_TAG);
                  when 'e' | 'E' =>
                     return Tag (Name, "template", TEMPLATE_TAG);
                  when others =>
                     return UNKNOWN_TAG;
               end case;

            when others =>
               return UNKNOWN_TAG;
            end case;
      end case;
   end Find_Tag;

end Wiki;
