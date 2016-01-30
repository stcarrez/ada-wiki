-----------------------------------------------------------------------
--  wiki -- Ada Wiki Engine
--  Copyright (C) 2016 Stephane Carrez
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
with Wiki.Attributes;
with Wiki.Documents;
package Wiki.Nodes is

   subtype Format_Map is Wiki.Documents.Format_Map;
   subtype WString is Wide_Wide_String;

   type Node_Kind is (N_HEADER,
                      N_LINE_BREAK,
                      N_HORIZONTAL_RULE,
                      N_PARAGRAPH,
                      N_BLOCKQUOTE,
                      N_QUOTE,
                      N_TAG_START,
                      N_TAG_END,
                      N_INDENT,
                      N_TEXT,
                      N_LINK,
                      N_IMAGE);

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

   type Node_List is limited private;
   type Node_List_Access is access all Node_List;

   type Node_Type (Kind : Node_Kind; Len : Natural) is limited record
      case Kind is
         when N_HEADER | N_BLOCKQUOTE | N_INDENT =>
            Level  : Natural := 0;
            Header : WString (1 .. Len);

         when N_TEXT =>
            Format : Format_Map;
            Text   : WString (1 .. Len);

         when N_LINK | N_IMAGE =>
            Link       : Wiki.Attributes.Attribute_List_Type;

         when N_QUOTE =>
            Quote      : WString (1 .. Len);

         when N_TAG_START =>
            Tag_Start  : Html_Tag_Type;
            Attributes : Wiki.Attributes.Attribute_List_Type;
            Children   : Node_List_Access;

         when N_TAG_END =>
            Tag_End    : Html_Tag_Type;

         when others =>
            null;

      end case;
   end record;
   type Node_Type_Access is access all Node_Type;

   type Document_Node_Access is private;
   type Document is limited private;

   --  Create a text node.
   function Create_Text (Text : in WString) return Node_Type_Access;

   --     procedure Add_Text (Doc  : in out Document;
--                         Text : in WString);

--     type Renderer is limited interface;
--
--     procedure Render (Engine : in out Renderer;
--                       Doc    : in Document;
--                       Node   : in Node_Type) is abstract;
--
--     procedure Iterate (Doc     : in Document;
--                        Process : access procedure (Doc : in Document; Node : in Node_Type)) is
--        Node : Document_Node_Access := Doc.First;
--     begin
--        while Node /= null loop
--           Process (Doc, Node.Data);
--           Node := Node.Next;
--        end loop;
--     end Iterate;

private

   NODE_LIST_BLOCK_SIZE : constant Positive := 20;

   type Node_Array is array (Positive range <>) of Node_Type_Access;

   type Node_List_Block;
   type Node_List_Block_Access is access all Node_List_Block;

   type Node_List_Block (Max : Positive) is limited record
      Next  : Node_List_Block_Access;
      Last  : Natural := 0;
      List  : Node_Array (1 .. Max);
   end record;

   type Node_List is limited record
      Current : Node_List_Block_Access;
      Length  : Natural := 0;
      First   : Node_List_Block (NODE_LIST_BLOCK_SIZE);
   end record;

   --  Append a node to the node list.
   procedure Append (Into : in out Node_List;
                     Node : in Node_Type_Access);

   type Document_Node;

   type Document_Node_Access is access all Document_Node;

   type Document_Node (Kind : Node_Kind; Len : Natural) is limited record
      Next  : Document_Node_Access;
      Prev  : Document_Node_Access;
      Data  : Node_Type (Kind, Len);
   end record;

   type Document is limited record
      First : Document_Node_Access;
      Last  : Document_Node_Access;
   end record;

end Wiki.Nodes;
