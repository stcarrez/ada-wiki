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
   procedure Add_Tag (Document   : in out Wiki.Nodes.Document;
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
   end Add_Tag;

   procedure End_Tag (Document : in out Wiki.Nodes.Document;
                      Tag      : in Html_Tag_Type) is
   begin
      if Document.Current /= null then
         Document.Current := Document.Current.Parent;
      end if;
   end End_Tag;

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
