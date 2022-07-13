-----------------------------------------------------------------------
--  wiki-filters-html -- Wiki HTML filters
--  Copyright (C) 2015, 2019, 2020, 2022 Stephane Carrez
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
with Wiki.Helpers;
package body Wiki.Filters.Html is

   --  ------------------------------
   --  Add a simple node such as N_LINE_BREAK, N_HORIZONTAL_RULE or N_PARAGRAPH to the document.
   --  ------------------------------
   overriding
   procedure Add_Node (Filter    : in out Html_Filter_Type;
                       Document  : in out Wiki.Documents.Document;
                       Kind      : in Wiki.Nodes.Simple_Node_Kind) is
      Tag : Html_Tag;
   begin
      case Kind is
         when N_LINE_BREAK =>
            Tag := BR_TAG;

         when N_PARAGRAPH =>
            Tag := P_TAG;

         when N_HORIZONTAL_RULE =>
            Tag := HR_TAG;

         when N_TOC_DISPLAY | N_NONE =>
            return;

         when N_NEWLINE | N_LIST_END | N_LIST_ITEM_END | N_NUM_LIST_END
           | N_LIST_ITEM | N_END_DEFINITION =>
            Filter_Type (Filter).Add_Node (Document, Kind);
            return;

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
                       Document  : in out Wiki.Documents.Document;
                       Text      : in Wiki.Strings.WString;
                       Format    : in Wiki.Format_Map) is
   begin
      if Filter.Hide_Level > 0 then
         return;
      end if;
      Filter_Type (Filter).Add_Text (Document, Text, Format);
   end Add_Text;

   --  ------------------------------
   --  Push a HTML node with the given tag to the document.
   --  ------------------------------
   overriding
   procedure Push_Node (Filter     : in out Html_Filter_Type;
                        Document   : in out Wiki.Documents.Document;
                        Tag        : in Wiki.Html_Tag;
                        Attributes : in out Wiki.Attributes.Attribute_List) is
      Current_Tag : Html_Tag;
   begin
      while not Filter.Stack.Is_Empty loop
         Current_Tag := Filter.Stack.Last_Element;
         if Wiki.Helpers.Need_Close (Tag, Current_Tag) then
            if Filter.Hide_Level = 0 then
               Filter_Type (Filter).Pop_Node (Document, Current_Tag);
            end if;
            Filter.Stack.Delete_Last;
         end if;
         exit;
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
                       Document : in out Wiki.Documents.Document;
                       Tag      : in Wiki.Html_Tag) is
      Current_Tag : Html_Tag;
   begin
      if Filter.Stack.Is_Empty then
         return;
      elsif not Filter.Allowed (Tag) and then not Filter.Hidden (Tag) then
         return;
      end if;

      --  Emit a end tag element until we find our matching tag and the top most tag
      --  allows the end tag to be omitted (ex: a td, tr, td, dd, ...).
      while not Filter.Stack.Is_Empty loop
         Current_Tag := Filter.Stack.Last_Element;
         exit when Current_Tag = Tag or else not Tag_Omission (Current_Tag);
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
                       Document   : in out Wiki.Documents.Document;
                       Name       : in Wiki.Strings.WString;
                       Attributes : in out Wiki.Attributes.Attribute_List) is
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
                        Document   : in out Wiki.Documents.Document;
                        Name       : in Wiki.Strings.WString;
                        Attributes : in out Wiki.Attributes.Attribute_List) is
   begin
      if Filter.Allowed (IMG_TAG) then
         Filter_Type (Filter).Add_Image (Document, Name, Attributes);
      end if;
   end Add_Image;

   --  ------------------------------
   --  Flush the HTML element that have not yet been closed.
   --  ------------------------------
   procedure Flush_Stack (Filter   : in out Html_Filter_Type;
                          Document : in out Wiki.Documents.Document) is
   begin
      while not Filter.Stack.Is_Empty loop
         declare
            Tag : constant Html_Tag := Filter.Stack.Last_Element;
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
                     Document : in out Wiki.Documents.Document) is
   begin
      Filter.Flush_Stack (Document);
      Filter_Type (Filter).Finish (Document);
   end Finish;

   --  ------------------------------
   --  Mark the HTML tag as being forbidden.
   --  ------------------------------
   procedure Forbidden (Filter : in out Html_Filter_Type;
                        Tag    : in Html_Tag) is
   begin
      Filter.Allowed (Tag) := False;
   end Forbidden;

   --  ------------------------------
   --  Mark the HTML tag as being allowed.
   --  ------------------------------
   procedure Allowed (Filter : in out Html_Filter_Type;
                      Tag    : in Html_Tag) is
   begin
      Filter.Allowed (Tag) := True;
   end Allowed;

   --  ------------------------------
   --  Mark the HTML tag as being hidden.  The tag and its inner content including the text
   --  will be removed and not passed to the final document.
   --  ------------------------------
   procedure Hide (Filter : in out Html_Filter_Type;
                   Tag    : in Html_Tag) is
   begin
      Filter.Hidden (Tag) := True;
   end Hide;

   --  ------------------------------
   --  Mark the HTML tag as being visible.
   --  ------------------------------
   procedure Visible (Filter : in out Html_Filter_Type;
                      Tag    : in Html_Tag) is
   begin
      Filter.Hidden (Tag) := False;
   end Visible;

end Wiki.Filters.Html;
