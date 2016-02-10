-----------------------------------------------------------------------
--  wiki-nodes -- Wiki Document Internal representation
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
with Ada.Wide_Wide_Characters.Handling;
package body Wiki.Nodes is

   type Tag_Array is array (Html_Tag_Type) of String_Access;

   HTML_TAG_NAME  : aliased constant String := "html";
   HEAD_TAG_NAME  : aliased constant String := "head";
   TITLE_TAG_NAME : aliased constant String := "title";
   BASE_TAG_NAME  : aliased constant String := "base";
   LINK_TAG_NAME  : aliased constant String := "link";
   META_TAG_NAME  : aliased constant String := "meta";
   STYLE_TAG_NAME : aliased constant String := "style";
   BODY_TAG_NAME  : aliased constant String := "body";
   ARTICLE_TAG_NAME : aliased constant String := "article";
   SECTION_TAG_NAME : aliased constant String := "section";
   NAV_TAG_NAME     : aliased constant String := "nav";
   ASIDE_TAG_NAME   : aliased constant String := "aside";

   Tag_Names : constant Tag_Array :=
     (
      HTML_TAG    => HTML_TAG_NAME'Access,
      HEAD_TAG    => HEAD_TAG_NAME'Access,
      TITLE_TAG   => TITLE_TAG_NAME'Access,
      BASE_TAG    => BASE_TAG_NAME'Access,
      LINK_TAG    => LINK_TAG_NAME'Access,
      META_TAG    => META_TAG_NAME'Access,
      STYLE_TAG   => STYLE_TAG_NAME'Access,
      BODY_TAG    => BODY_TAG_NAME'Access,
      ARTICLE_TAG => ARTICLE_TAG_NAME'Access,
      SECTION_TAG => SECTION_TAG_NAME'Access,
      NAV_TAG     => NAV_TAG_NAME'Access,
      ASIDE_TAG   => ASIDE_TAG_NAME'Access,
      others => null
     );

   --  ------------------------------
   --  Get the HTML tag name.
   --  ------------------------------
   function Get_Tag_Name (Tag : in Html_Tag_Type) return String_Access is
   begin
      return Tag_Names (Tag);
   end Get_Tag_Name;

   --  ------------------------------
   --  Find the tag from the tag name.
   --  ------------------------------
   function Find_Tag (Name : in Wide_Wide_String) return Html_Tag_Type is
      function Tag (Name   : in Wide_Wide_String;
                    Expect : in Wide_Wide_String;
                    Tag    : in Html_Tag_Type) return Html_Tag_Type is
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
                  when 'n' | 'N' =>
                     return SPAN_TAG;
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
                     return Tag (Name, "html", HTML_TAG);
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

   --  ------------------------------
   --  Create a text node.
   --  ------------------------------
   function Create_Text (Text : in WString) return Node_Type_Access is
   begin
      return new Node_Type '(Kind => N_TEXT,
                             Len  => Text'Length,
                             Text => Text,
                             others => <>);
   end Create_Text;

   --  ------------------------------
   --  Append a HTML tag start node to the document.
   --  ------------------------------
   procedure Push_Node (Document   : in out Wiki.Nodes.Document;
                        Tag        : in Html_Tag_Type;
                        Attributes : in Wiki.Attributes.Attribute_List_Type) is
      Node : constant Node_Type_Access := new Node_Type '(Kind       => N_TAG_START,
                                                          Len        => 0,
                                                          Tag_Start  => Tag,
                                                          Attributes => Attributes,
                                                          Children   => null,
                                                          Parent     => Document.Current);
   begin
      Append (Document.Nodes, Node);
      Document.Current := Node;
   end Push_Node;

   --  ------------------------------
   --  Pop the HTML tag.
   --  ------------------------------
   procedure Pop_Node (Document : in out Wiki.Nodes.Document;
                       Tag      : in Html_Tag_Type) is
   begin
      if Document.Current /= null then
         Document.Current := Document.Current.Parent;
      end if;
   end Pop_Node;

   --  ------------------------------
   --  Append a section header at end of the document.
   --  ------------------------------
   procedure Append (Into   : in out Document;
                     Header : in Wiki.Strings.WString;
                     Level  : in Positive) is
   begin
      Append (Into, new Node_Type '(Kind   => N_HEADER,
                                    Len    => Header'Length,
                                    Header => Header,
                                    Level  => Level));
   end Append;

   --  ------------------------------
   --  Append a node to the document.
   --  ------------------------------
   procedure Append (Into : in out Document;
                     Node : in Node_Type_Access) is
   begin
      if Into.Current = null then
         Append (Into.Nodes, Node);
      else
         if Into.Current.Children = null then
            Into.Current.Children := new Node_List;
         end if;
         Append (Into.Current.Children.all, Node);
      end if;
   end Append;

   --  ------------------------------
   --  Append a simple node such as N_LINE_BREAK, N_HORIZONTAL_RULE or N_PARAGRAPH.
   --  ------------------------------
   procedure Append (Into : in out Document;
                     Kind : in Simple_Node_Kind) is
   begin
      case Kind is
         when N_LINE_BREAK =>
            Append (Into, new Node_Type '(Kind => N_LINE_BREAK, Len => 0));

         when N_HORIZONTAL_RULE =>
            Append (Into, new Node_Type '(Kind => N_HORIZONTAL_RULE, Len => 0));

         when N_PARAGRAPH =>
            Append (Into, new Node_Type '(Kind => N_PARAGRAPH, Len => 0));

      end  case;
   end Append;

   --  ------------------------------
   --  Append the text with the given format at end of the document.
   --  ------------------------------
   procedure Append (Into   : in out Document;
                     Text   : in Wiki.Strings.WString;
                     Format : in Format_Map) is
   begin
      Append (Into, new Node_Type '(Kind => N_TEXT, Len => Text'Length,
                                    Text => Text, Format => Format));
   end Append;

   --  ------------------------------
   --  Add a link.
   --  ------------------------------
   procedure Add_Link (Into       : in out Document;
                       Name       : in Wiki.Strings.WString;
                       Attributes : in out Wiki.Attributes.Attribute_List_Type) is
   begin
      Append (Into, new Node_Type '(Kind => N_LINK, Len => Name'Length,
                                    Title => Name, Link_Attr => Attributes));
   end Add_Link;

   --  ------------------------------
   --  Add an image.
   --  ------------------------------
   procedure Add_Image (Into       : in out Document;
                        Name       : in Wiki.Strings.WString;
                        Attributes : in out Wiki.Attributes.Attribute_List_Type) is
   begin
      Append (Into, new Node_Type '(Kind => N_IMAGE, Len => Name'Length,
                                    Title => Name, Link_Attr => Attributes));
   end Add_Image;

   --  ------------------------------
   --  Add a quote.
   --  ------------------------------
   procedure Add_Quote (Into       : in out Document;
                        Name       : in Wiki.Strings.WString;
                        Attributes : in out Wiki.Attributes.Attribute_List_Type) is
   begin
      Append (Into, new Node_Type '(Kind => N_QUOTE, Len => Name'Length,
                                    Title => Name, Link_Attr => Attributes));
   end Add_Quote;

   --  ------------------------------
   --  Add a blockquote (<blockquote>).  The level indicates the blockquote nested level.
   --  The blockquote must be closed at the next header.
   --  ------------------------------
   procedure Add_Blockquote (Into     : in out Document;
                             Level    : in Natural) is
   begin
      Append (Into, new Node_Type '(Kind => N_BLOCKQUOTE, Len => 0,
                                    Level => Level, others => <>));
   end Add_Blockquote;

   --  ------------------------------
   --  Append a node to the node list.
   --  ------------------------------
   procedure Append (Into : in out Node_List;
                     Node : in Node_Type_Access) is
      Block : Node_List_Block_Access := Into.Current;
   begin
      if Block.Last = Block.Max then
         Block.Next := new Node_List_Block (Into.Length);
         Block := Block.Next;
         Into.Current := Block;
      end if;
      Block.Last := Block.Last + 1;
      Block.List (Block.Last) := Node;
      Into.Length := Into.Length + 1;
   end Append;

end Wiki.Nodes;
