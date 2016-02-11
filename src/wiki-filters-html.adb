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
with Ada.Wide_Wide_Characters.Handling;
package body Wiki.Filters.Html is

   function Tag (Name   : in Wide_Wide_String;
                 Expect : in Wide_Wide_String;
                 Tag    : in Html_Tag_Type) return Html_Tag_Type;

   function Need_Close (Tag         : in Html_Tag_Type;
                        Current_Tag : in Html_Tag_Type) return Boolean;

   type Tag_Name_Array is array (Html_Tag_Type) of Unbounded_Wide_Wide_String;

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

   Tag_Names : constant Tag_Name_Array
     := (

         B_TAG     => To_Unbounded_Wide_Wide_String ("b"),
         I_TAG     => To_Unbounded_Wide_Wide_String ("i"),
         U_TAG     => To_Unbounded_Wide_Wide_String ("u"),
         IMG_TAG   => To_Unbounded_Wide_Wide_String ("img"),
         HR_TAG    => To_Unbounded_Wide_Wide_String ("hr"),
         BR_TAG    => To_Unbounded_Wide_Wide_String ("br"),
         SPAN_TAG  => To_Unbounded_Wide_Wide_String ("span"),
         OPTION_TAG => To_Unbounded_Wide_Wide_String ("option"),
         DL_TAG    => To_Unbounded_Wide_Wide_String ("dl"),
         DT_TAG    => To_Unbounded_Wide_Wide_String ("dt"),
         DD_TAG    => To_Unbounded_Wide_Wide_String ("dd"),
         TABLE_TAG => To_Unbounded_Wide_Wide_String ("table"),
         TBODY_TAG => To_Unbounded_Wide_Wide_String ("tbody"),
         THEAD_TAG => To_Unbounded_Wide_Wide_String ("thead"),
         TFOOT_TAG => To_Unbounded_Wide_Wide_String ("tfoot"),
         TH_TAG    => To_Unbounded_Wide_Wide_String ("th"),
         TR_TAG    => To_Unbounded_Wide_Wide_String ("tr"),
         TD_TAG    => To_Unbounded_Wide_Wide_String ("td"),
         others    => Null_Unbounded_Wide_Wide_String);

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

   function Find_Tag (Name : in Unbounded_Wide_Wide_String) return Html_Tag_Type is
   begin
      return Find_Tag (To_Wide_Wide_String (Name));
   end Find_Tag;

   --  ------------------------------
   --  Find the tag from the tag name.
   --  ------------------------------
   function Find_Tag (Name : in Wide_Wide_String) return Html_Tag_Type is
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
   --  Add a simple node such as N_LINE_BREAK, N_HORIZONTAL_RULE or N_PARAGRAPH to the document.
   --  ------------------------------
   overriding
   procedure Add_Node (Filter    : in out Html_Filter_Type;
                       Document  : in out Wiki.Nodes.Document;
                       Kind      : in Wiki.Nodes.Simple_Node_Kind) is
     Tag : Html_Tag_Type;
   begin
     case Kind is
        when N_LINE_BREAK =>
           Tag := BR_TAG;

        when N_PARAGRAPH =>
           Tag := P_TAG;

        when N_HORIZONTAL_RULE =>
           Tag := HR_TAG;

     end case;
     if Filter.Allowed (Tag) then
        Filter_Type (Filter).Add_Node (Document, Kind);
     end if;
   end Add_Node;

   --  ------------------------------
   --  Add a text content with the given format to the document.
   --  ------------------------------
   overriding
   procedure Add_Text (Filter    : in out Html_Filter_Type;
                       Document  : in out Wiki.Nodes.Document;
                       Text      : in Wiki.Strings.WString;
                       Format    : in Wiki.Nodes.Format_Map) is
   begin
      if Filter.Hide_Level > 0 then
         return;
      elsif not Filter.Stack.Is_Empty then
         declare
            Current_Tag : constant Html_Tag_Type := Filter.Stack.Last_Element;
         begin
            if No_End_Tag (Current_Tag) then
               Filter_Type (Filter).Pop_Node (Document, Current_Tag);
               Filter.Stack.Delete_Last;
            end if;
         end;
      end if;
      Filter_Type (Filter).Add_Text (Document, Text, Format);
   end Add_Text;

   --  ------------------------------
   --  Add a section header with the given level in the document.
   --  ------------------------------
   overriding
   procedure Add_Header (Filter    : in out Html_Filter_Type;
                         Document  : in out Wiki.Nodes.Document;
                         Header    : in Wiki.Strings.WString;
                         Level     : in Natural) is
   begin
      Filter.Flush_Stack (Document);
      Filter_Type (Filter).Add_Header (Document, Header, Level);
   end Add_Header;

   --  ------------------------------
   --  Push a HTML node with the given tag to the document.
   --  ------------------------------
   overriding
   procedure Push_Node (Filter     : in out Html_Filter_Type;
                        Document   : in out Wiki.Nodes.Document;
                        Tag        : in Wiki.Nodes.Html_Tag_Type;
                        Attributes : in out Wiki.Attributes.Attribute_List_Type) is
      Current_Tag : Html_Tag_Type;
   begin
      while not Filter.Stack.Is_Empty loop
         Current_Tag := Filter.Stack.Last_Element;
         if Need_Close (Tag, Current_Tag) then
            if Filter.Hide_Level = 0 then
               Filter_Type (Filter).Pop_Node (Document, Current_Tag);
            end if;
            Filter.Stack.Delete_Last;
         end if;
         exit when not No_End_Tag (Current_Tag);
      end loop;
      if Filter.Hidden (Tag) then
         Filter.Hide_Level := Filter.Hide_Level + 1;
      elsif not Filter.Allowed (Tag) then
         return;
      end if;
      Filter.Stack.Append (Tag);
      if Filter.Hide_Level = 0 then
         Filter_Type (Filter).Push_Node (Document, Tag, Attributes);
      end if;
   end Push_Node;

   --  ------------------------------
   --  Pop a HTML node with the given tag.
   --  ------------------------------
   overriding
   procedure Pop_Node (Filter   : in out Html_Filter_Type;
                       Document : in out Wiki.Nodes.Document;
                       Tag      : in Wiki.Nodes.Html_Tag_Type) is
      Current_Tag : Html_Tag_Type;
   begin
      if Filter.Stack.Is_Empty then
         return;
      elsif not Filter.Allowed (Tag) and not Filter.Hidden (Tag) then
         return;
      end if;

      --  Emit a end tag element until we find our matching tag and the top most tag
      --  allows the end tag to be omitted (ex: a td, tr, td, dd, ...).
      while not Filter.Stack.Is_Empty loop
         Current_Tag := Filter.Stack.Last_Element;
         exit when Current_Tag = Tag or not Tag_Omission (Current_Tag);
         if Filter.Hide_Level = 0 then
            Filter_Type (Filter).Pop_Node (Document, Current_Tag);
         end if;
         Filter.Stack.Delete_Last;
      end loop;

      if Filter.Hide_Level = 0 then
         Filter_Type (Filter).Pop_Node (Document, Tag);
      end if;
      if Filter.Hidden (Current_Tag) then
         Filter.Hide_Level := Filter.Hide_Level - 1;
      end if;
      Filter.Stack.Delete_Last;
   end Pop_Node;

   --  ------------------------------
   --  Add a link.
   --  ------------------------------
   overriding
   procedure Add_Link (Filter     : in out Html_Filter_Type;
                       Document   : in out Wiki.Nodes.Document;
                       Name       : in Wiki.Strings.WString;
                       Attributes : in out Wiki.Attributes.Attribute_List_Type) is
   begin
      if Filter.Allowed (A_TAG) then
         Filter_Type (Filter).Add_Link (Document, Name, Attributes);
      end if;
   end Add_Link;

   --  ------------------------------
   --  Add an image.
   --  ------------------------------
   overriding
   procedure Add_Image (Filter     : in out Html_Filter_Type;
                        Document   : in out Wiki.Nodes.Document;
                        Name       : in Wiki.Strings.WString;
                        Attributes : in out Wiki.Attributes.Attribute_List_Type) is
   begin
      if Filter.Allowed (IMG_TAG) then
         Filter_Type (Filter).Add_Image (Document, Name, Attributes);
      end if;
   end Add_Image;

   --  ------------------------------
   --  Given the current tag on the top of the stack and the new tag that will be pushed,
   --  decide whether the current tag must be closed or not.
   --  Returns True if the current tag must be closed.
   --  ------------------------------
   function Need_Close (Tag         : in Html_Tag_Type;
                        Current_Tag : in Html_Tag_Type) return Boolean is
   begin
      if No_End_Tag (Current_Tag) then
         return True;
      elsif Current_Tag = Tag and Tag_Omission (Current_Tag) then
         return True;
      else
         case Current_Tag is
            when DT_TAG | DD_TAG =>
               return Tag = DD_TAG or Tag = DL_TAG or Tag = DT_TAG;

            when TD_TAG =>
               return Tag = TD_TAG or Tag = TR_TAG or Tag = TH_TAG;

            when TR_TAG =>
               return False;

            when others =>
               return False;

         end case;
      end if;
   end Need_Close;

   --  ------------------------------
   --  Flush the HTML element that have not yet been closed.
   --  ------------------------------
   procedure Flush_Stack (Filter   : in out Html_Filter_Type;
                          Document : in out Wiki.Nodes.Document) is
   begin
      while not Filter.Stack.Is_Empty loop
         declare
            Tag : constant Html_Tag_Type := Filter.Stack.Last_Element;
         begin
            if Filter.Hide_Level = 0 then
               Filter_Type (Filter).Pop_Node (Document, Tag);
            end if;
            if Filter.Hidden (Tag) then
               Filter.Hide_Level := Filter.Hide_Level - 1;
            end if;
         end;
         Filter.Stack.Delete_Last;
      end loop;
   end Flush_Stack;

   --  ------------------------------
   --  Finish the document after complete wiki text has been parsed.
   --  ------------------------------
   overriding
   procedure Finish (Filter : in out Html_Filter_Type;
                     Document : in out Wiki.Nodes.Document) is
   begin
      Filter.Flush_Stack (Document);
      Filter_Type (Filter).Finish (Document);
   end Finish;

   --  ------------------------------
   --  Mark the HTML tag as being forbidden.
   --  ------------------------------
   procedure Forbidden (Filter : in out Html_Filter_Type;
                        Tag    : in Html_Tag_Type) is
   begin
      Filter.Allowed (Tag) := False;
   end Forbidden;

   --  ------------------------------
   --  Mark the HTML tag as being allowed.
   --  ------------------------------
   procedure Allowed (Filter : in out Html_Filter_Type;
                      Tag    : in Html_Tag_Type) is
   begin
      Filter.Allowed (Tag) := True;
   end Allowed;

   --  ------------------------------
   --  Mark the HTML tag as being hidden.  The tag and its inner content including the text
   --  will be removed and not passed to the final document.
   --  ------------------------------
   procedure Hide (Filter : in out Html_Filter_Type;
                   Tag    : in Html_Tag_Type) is
   begin
      Filter.Hidden (Tag) := True;
   end Hide;

   --  ------------------------------
   --  Mark the HTML tag as being visible.
   --  ------------------------------
   procedure Visible (Filter : in out Html_Filter_Type;
                      Tag    : in Html_Tag_Type) is
   begin
      Filter.Hidden (Tag) := False;
   end Visible;

end Wiki.Filters.Html;
