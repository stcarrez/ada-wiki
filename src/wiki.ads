-----------------------------------------------------------------------
--  wiki -- Ada Wiki Engine
--  Copyright (C) 2015, 2016, 2020, 2021, 2022 Stephane Carrez
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

--  = Wiki =
--  The Wiki engine parses a Wiki text in several Wiki syntax such as `MediaWiki`,
--  `Creole`, `Markdown`, `Dotclear` and renders the result either in HTML, text or into
--  another Wiki format.  The Wiki engine is used in two steps:
--
--  * The Wiki text is parsed according to its syntax to produce a Wiki Document instance.
--  * The Wiki document is then rendered by a renderer to produce the final HTML, text.
--
--  Through this process, it is possible to insert filters and plugins to customize the
--  parsing and the rendering.
--
--  [images/ada-wiki.png]
--
--  The Ada Wiki engine is organized in several packages:
--
--  * The [Wiki Streams](#wiki-streams) packages define the interface, types and operations
--  for the Wiki engine to read the Wiki or HTML content and for the Wiki renderer to generate
--  the HTML or text outputs.
--  * The [Wiki parser](#wiki-parsers) is responsible for parsing HTML or Wiki content
--  according to a selected Wiki syntax.  It builds the final Wiki document through filters
--  and plugins.
--  * The [Wiki Filters](#wiki-filters) provides a simple filter framework that allows to plug
--  specific filters when a Wiki document is parsed and processed.  Filters are used for the
--  table of content generation, for the HTML filtering, to collect words or links
--  and so on.
--  * The [Wiki Plugins](#wiki-plugins) defines the plugin interface that is used
--  by the Wiki engine to provide pluggable extensions in the Wiki.  Plugins are used
--  for the Wiki template support, to hide some Wiki text content when it is rendered
--  or to interact with other systems.
--  * The Wiki documents and attributes are used for the representation of the Wiki
--  document after the Wiki content is parsed.
--  * The [Wiki renderers](@wiki-render) are the last packages which are used for the rendering
--  of the Wiki document to produce the final HTML or text.
--
--  @include-doc docs/Tutorial.md
--  @include wiki-documents.ads
--  @include wiki-attributes.ads
--  @include wiki-parsers.ads
--  @include wiki-filters.ads
--  @include wiki-plugins.ads
--  @include wiki-render.ads
--  @include wiki-streams.ads
package Wiki is

   pragma Preelaborate;

   --  Defines the possible wiki syntax supported by the parser.
   type Wiki_Syntax
      is (
         --  Google wiki syntax http://code.google.com/p/support/wiki/WikiSyntax
         SYNTAX_GOOGLE,

         --  Creole wiki syntax http://www.wikicreole.org/wiki/Creole1.0
         SYNTAX_CREOLE,

         --  Dotclear syntax http://dotclear.org/documentation/2.0/usage/syntaxes
         SYNTAX_DOTCLEAR,

         --  PhpBB syntax http://wiki.phpbb.com/Help:Formatting
         SYNTAX_PHPBB,

         --  MediaWiki syntax http://www.mediawiki.org/wiki/Help:Formatting
         SYNTAX_MEDIA_WIKI,

         --  Markdown
         SYNTAX_MARKDOWN,

         --  Textile syntax
         --  https://www.redmine.org/projects/redmine/wiki/RedmineTextFormattingTextile
         SYNTAX_TEXTILE,

         --  The input is plain possibly incorrect HTML.
         SYNTAX_HTML);

   --  Defines the possible text formats.
   type Format_Type is (BOLD, STRONG, ITALIC, EMPHASIS, CODE, SUPERSCRIPT, SUBSCRIPT, STRIKEOUT,
                        PREFORMAT, INS, UNDERLINE, CITE);

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

      --  Deprecated tags but still used widely
      TT_TAG,

      --  Unknown tags
      UNKNOWN_TAG
     );

   --  Find the tag from the tag name.
   function Find_Tag (Name : in Wide_Wide_String) return Html_Tag;

   type String_Access is access constant String;

   --  Get the HTML tag name.
   function Get_Tag_Name (Tag : in Html_Tag) return String_Access;

   type Tag_Boolean_Array is array (Html_Tag) of Boolean;

   No_End_Tag   : constant Tag_Boolean_Array :=
     (
      BASE_TAG   => True,
      LINK_TAG   => True,
      META_TAG   => True,
      IMG_TAG    => True,
      HR_TAG     => True,
      BR_TAG     => True,
      WBR_TAG    => True,
      INPUT_TAG  => True,
      KEYGEN_TAG => True,
      others     => False);

   Tag_Omission : constant Tag_Boolean_Array :=
     (
      --  Section 4.4 Grouping content
      LI_TAG    => True,
      DT_TAG    => True,
      DD_TAG    => True,

      --  Section 4.5 Text-level semantics
      RB_TAG    => True,
      RT_TAG    => True,
      RTC_TAG   => True,
      RP_TAG    => True,

      --  Section 4.9 Tabular data
      TH_TAG    => True,
      TD_TAG    => True,
      TR_TAG    => True,
      TBODY_TAG => True,
      THEAD_TAG => True,
      TFOOT_TAG => True,

      OPTGROUP_TAG => True,
      OPTION_TAG   => True,

      others    => False);

   --  Tags before and after which we want to preserve spaces.
   Tag_Text : constant Tag_Boolean_Array :=
     (
       A_TAG      => True,
       EM_TAG     => True,
       STRONG_TAG => True,
       SMALL_TAG  => True,
       S_TAG      => True,
       CITE_TAG   => True,
       Q_TAG      => True,
       DFN_TAG    => True,
       ABBR_TAG   => True,
       TIME_TAG   => True,
       CODE_TAG   => True,
       VAR_TAG    => True,
       SAMP_TAG   => True,
       KBD_TAG    => True,
       SUB_TAG    => True,
       SUP_TAG    => True,
       I_TAG      => True,
       B_TAG      => True,
       MARK_TAG   => True,
       RUBY_TAG   => True,
       RT_TAG     => True,
       RP_TAG     => True,
       BDI_TAG    => True,
       BDO_TAG    => True,
       SPAN_TAG   => True,
       INS_TAG    => True,
       DEL_TAG    => True,
       TT_TAG     => True,
       UNKNOWN_TAG => True,
       others     => False
     );

end Wiki;
