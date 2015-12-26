-----------------------------------------------------------------------
--  wiki-filters-html -- Wiki HTML filters
--  Copyright (C) 2015 Stephane Carrez
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
private with Ada.Containers.Vectors;

--  === HTML Filters ===
--  The <b>Wiki.Filters.Html</b> package implements a customizable HTML filter that verifies
--  the HTML content embedded in the Wiki text.
--
--  The HTML filter may be declared and configured as follows:
--
--    F : aliased Wiki.Filters.Html.Html_Filter_Type;
--    ...
--    F.Forbidden (Wiki.Filters.Html.SCRIPT_TAG);
--    F.Forbidden (Wiki.Filters.Html.A_TAG);
--
--  The <tt>Set_Document</tt> operation is used to link the HTML filter to a next filter
--  or to the HTML or text renderer:
--
--    F.Set_Document (Renderer'Access);
--
--  The HTML filter is then either inserted as a document for a previous filter or for
--  the wiki parser:
--
--    Wiki.Parsers.Parse (F'Access, Wiki_Text, Syntax);
--
package Wiki.Filters.Html is

   --  The possible HTML tags as described in HTML5 specification.
   type Html_Tag_Type is
     (
      --  Section 4.1 The root element
      HTML_TAG,

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

   --  ------------------------------
   --  Filter type
   --  ------------------------------
   type Html_Filter_Type is new Filter_Type with private;

   --  Add a section header in the document.
   overriding
   procedure Add_Header (Document : in out Html_Filter_Type;
                         Header   : in Unbounded_Wide_Wide_String;
                         Level    : in Positive);

   --  Add a text block with the given format.
   overriding
   procedure Add_Text (Document : in out Html_Filter_Type;
                       Text     : in Unbounded_Wide_Wide_String;
                       Format   : in Wiki.Documents.Format_Map);

   --  Add a link.
   overriding
   procedure Add_Link (Document : in out Html_Filter_Type;
                       Name     : in Unbounded_Wide_Wide_String;
                       Link     : in Unbounded_Wide_Wide_String;
                       Language : in Unbounded_Wide_Wide_String;
                       Title    : in Unbounded_Wide_Wide_String);

   --  Add an image.
   overriding
   procedure Add_Image (Document    : in out Html_Filter_Type;
                        Link        : in Unbounded_Wide_Wide_String;
                        Alt         : in Unbounded_Wide_Wide_String;
                        Position    : in Unbounded_Wide_Wide_String;
                        Description : in Unbounded_Wide_Wide_String);

   overriding
   procedure Start_Element (Document   : in out Html_Filter_Type;
                            Name       : in Unbounded_Wide_Wide_String;
                            Attributes : in Wiki.Attributes.Attribute_List_Type);

   overriding
   procedure End_Element (Document : in out Html_Filter_Type;
                          Name     : in Unbounded_Wide_Wide_String);

   --  Finish the document after complete wiki text has been parsed.
   overriding
   procedure Finish (Document : in out Html_Filter_Type);

   --  Mark the HTML tag as being forbidden.
   procedure Forbidden (Filter : in out Html_Filter_Type;
                        Tag    : in Html_Tag_Type);

   --  Mark the HTML tag as being allowed.
   procedure Allowed (Filter : in out Html_Filter_Type;
                      Tag    : in Html_Tag_Type);

   --  Flush the HTML element that have not yet been closed.
   procedure Flush_Stack (Document : in out Html_Filter_Type);

private

   type Tag_Boolean_Array is array (Html_Tag_Type) of Boolean;

   package Tag_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => Html_Tag_Type);

   subtype Tag_Vector is Tag_Vectors.Vector;
   subtype Tag_Cursor is Tag_Vectors.Cursor;

   type Html_Filter_Type is new Filter_Type with record
      Allowed  : Tag_Boolean_Array := (UNKNOWN_TAG => False,
                                       SCRIPT_TAG  => False,
                                       HTML_TAG    => False,
                                       HEAD_TAG    => False,
                                       BODY_TAG    => False,
                                       META_TAG    => False,
                                       TITLE_TAG   => False,
                                       others      => True);
      Stack    : Tag_Vector;
   end record;

   --  Find the tag from the tag name.
   function Find_Tag (Name : in Wide_Wide_String) return Html_Tag_Type;

end Wiki.Filters.Html;
