-----------------------------------------------------------------------
--  wiki-nodes -- Wiki Document Internal representation
--  Copyright (C) 2016, 2019, 2020, 2022, 2024, 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Wide_Wide_Characters.Handling;
package body Wiki.Documents is

   use Wiki.Nodes;
   use Wiki.Nodes.Lists;

   --  ------------------------------
   --  Append a HTML tag start node to the document.
   --  ------------------------------
   procedure Start_Block (Into  : in out Document;
                          Kind  : in Wiki.Nodes.Node_Kind;
                          Level : in Natural) is
      Node : Node_Type_Access;
   begin
      case Kind is
         when N_HEADER =>
            Node := new Node_Type '(Kind       => N_HEADER,
                                    Len        => 0,
                                    Level      => Level,
                                    Loose      => False,
                                    Children   => null,
                                    Parent     => Into.Current);

         when N_DEFINITION_TERM =>
            Node := new Node_Type '(Kind       => N_DEFINITION_TERM,
                                    Len        => 0,
                                    Level      => Level,
                                    Loose      => False,
                                    Children   => null,
                                    Parent     => Into.Current);

         when N_DEFINITION =>
            Node := new Node_Type '(Kind       => N_DEFINITION,
                                    Len        => 0,
                                    Level      => Level,
                                    Loose      => False,
                                    Children   => null,
                                    Parent     => Into.Current);

         when N_LIST_ITEM =>
            Node := new Node_Type '(Kind       => N_LIST_ITEM,
                                    Len        => 0,
                                    Children   => null,
                                    Parent     => Into.Current);

         when others =>
            return;

      end case;
      Append_Push (Into, Node);
   end Start_Block;

   procedure End_Block (From : in out Document;
                        Kind  : in Wiki.Nodes.Node_Kind) is
      pragma Unreferenced (Kind);
   begin
      if From.Current /= null then
         From.Current := From.Current.Parent;
      end if;
   end End_Block;

   --  ------------------------------
   --  Append a HTML tag start node to the document.
   --  ------------------------------
   procedure Push_Node (Into       : in out Document;
                        Tag        : in Html_Tag;
                        Attributes : in Wiki.Attributes.Attribute_List) is
      Node : Node_Type_Access;
   begin
      if Tag = A_TAG then
         Node := new Node_Type '(Kind => N_LINK, Len => 0,
                                 Children => null,
                                 Link_Attr => Attributes,
                                 Title => "",
                                 Parent => Into.Current);
      else
         Node := new Node_Type '(Kind       => N_TAG_START,
                                 Len        => 0,
                                 Tag_Start  => Tag,
                                 Attributes => Attributes,
                                 Children   => null,
                                 Parent     => Into.Current);
      end if;
      Append_Push (Into, Node);
   end Push_Node;

   --  ------------------------------
   --  Pop the HTML tag.
   --  ------------------------------
   procedure Pop_Node (From : in out Document;
                       Tag  : in Html_Tag) is
      pragma Unreferenced (Tag);
   begin
      if From.Current /= null then
         From.Current := From.Current.Parent;
      end if;
   end Pop_Node;

   --  ------------------------------
   --  Returns True if the current node is the root document node.
   --  ------------------------------
   function Is_Root_Node (Doc : in Document) return Boolean is
   begin
      return Doc.Current = null;
   end Is_Root_Node;

   --  ------------------------------
   --  Append a node to the document.
   --  ------------------------------
   procedure Append (Into : in out Document;
                     Node : in Wiki.Nodes.Node_Type_Access) is
   begin
      if Into.Current = null then
         Append (Into.Nodes, Node);
      else
         Append (Into.Current, Node);
      end if;
   end Append;

   --  ------------------------------
   --  Append and push the node to the document.
   --  ------------------------------
   procedure Append_Push (Into : in out Document;
                          Node : in Wiki.Nodes.Node_Type_Access) is
   begin
      if Into.Current = null then
         Append (Into.Nodes, Node);
      else
         Append (Into.Current, Node);
      end if;
      Into.Current := Node;
   end Append_Push;

   --  ------------------------------
   --  Append a simple node such as N_LINE_BREAK, N_HORIZONTAL_RULE or N_PARAGRAPH.
   --  ------------------------------
   procedure Append (Into : in out Document;
                     Kind : in Simple_Node_Kind) is
   begin
      case Kind is
         when N_LINE_BREAK =>
            Append (Into, new Node_Type '(Kind => N_LINE_BREAK, Len => 0,
                                          Children => null,
                                          Parent => Into.Current));

         when N_HORIZONTAL_RULE =>
            Append (Into, new Node_Type '(Kind => N_HORIZONTAL_RULE, Len => 0,
                                          Children => null,
                                          Parent => Into.Current));

         when N_PARAGRAPH =>
            Append (Into, new Node_Type '(Kind => N_PARAGRAPH, Len => 0,
                                          Children => null,
                                          Parent => Into.Current));

         when N_END_DEFINITION =>
            Append (Into, new Node_Type '(Kind => N_END_DEFINITION, Len => 0,
                                          Children => null,
                                          Parent => Into.Current));

         when N_TOC_DISPLAY =>
            Append (Into, new Node_Type '(Kind => N_TOC_DISPLAY, Len => 0,
                                          Children => null,
                                          Parent => Into.Current));
            Into.Using_TOC := True;

         when N_NONE =>
            null;

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
                                    Parent => Into.Current,
                                    Children => null,
                                    Text => Text, Format => Format));
   end Append;

   --  ------------------------------
   --  Add a link.
   --  ------------------------------
   procedure Add_Link (Into          : in out Document;
                       Name          : in Wiki.Strings.WString;
                       Attributes    : in out Wiki.Attributes.Attribute_List;
                       Reference     : in Boolean;
                       With_Children : in Boolean) is
      Node : Wiki.Nodes.Node_Type_Access;
   begin
      if Reference then
         Node := new Node_Type '(Kind => N_LINK_REF, Len => Name'Length,
                                 Parent => Into.Current,
                                 Children => null,
                                 Title => Name, Link_Attr => Attributes);
      else
         Node := new Node_Type '(Kind => N_LINK, Len => Name'Length,
                                 Parent => Into.Current,
                                 Children => null,
                                 Title => Name, Link_Attr => Attributes);
      end if;
      Append (Into, Node);
      if With_Children then
         Into.Current := Node;
      end if;
   end Add_Link;

   --  ------------------------------
   --  Add an image.
   --  ------------------------------
   procedure Add_Image (Into       : in out Document;
                        Name       : in Wiki.Strings.WString;
                        Attributes : in out Wiki.Attributes.Attribute_List;
                        Reference  : in Boolean) is
   begin
      if Reference then
         Append (Into, new Node_Type '(Kind => N_IMAGE_REF, Len => Name'Length,
                                       Parent => Into.Current,
                                       Children => null,
                                       Title => Name, Link_Attr => Attributes));
      else
         Append (Into, new Node_Type '(Kind => N_IMAGE, Len => Name'Length,
                                       Parent => Into.Current,
                                       Children => null,
                                       Title => Name, Link_Attr => Attributes));
      end if;
   end Add_Image;

   --  ------------------------------
   --  Add a quote.
   --  ------------------------------
   procedure Add_Quote (Into       : in out Document;
                        Name       : in Wiki.Strings.WString;
                        Attributes : in out Wiki.Attributes.Attribute_List) is
   begin
      Append (Into, new Node_Type '(Kind => N_QUOTE, Len => Name'Length,
                                    Parent => Into.Current,
                                    Children => null,
                                    Title => Name, Link_Attr => Attributes));
   end Add_Quote;

   --  ------------------------------
   --  Add a list (<ul> or <ol>) starting at the given number.
   --  ------------------------------
   procedure Add_List (Into     : in out Document;
                       Level    : in Natural;
                       Ordered  : in Boolean) is
      List : Node_Type_Access;
   begin
      if Ordered then
         List := new Node_Type '(Kind => N_NUM_LIST_START, Len => 0,
                                 Parent => Into.Current,
                                 Children => null,
                                 Level => Level,
                                 Loose => False);
      else
         List := new Node_Type '(Kind => N_LIST_START, Len => 0,
                                 Parent => Into.Current,
                                 Children => null,
                                 Level => Level,
                                 Loose => False);
      end if;
      Append_Push (Into, List);
   end Add_List;

   --  ------------------------------
   --  Add a blockquote (<blockquote>).  The level indicates the blockquote nested level.
   --  The blockquote must be closed at the next header.
   --  ------------------------------
   procedure Add_Blockquote (Into     : in out Document;
                             Level    : in Natural) is
   begin
      Append (Into, new Node_Type '(Kind => N_BLOCKQUOTE, Len => 0,
                                    Parent => Into.Current,
                                    Children => null,
                                    Level => Level,
                                    Loose => False));
   end Add_Blockquote;

   --  ------------------------------
   --  Add a text block that is pre-formatted.
   --  ------------------------------
   procedure Add_Preformatted (Into     : in out Document;
                               Text     : in Wiki.Strings.WString;
                               Format   : in Wiki.Strings.WString) is
   begin
      Append (Into, new Node_Type '(Kind => N_PREFORMAT, Len => Text'Length,
                                    Parent => Into.Current,
                                    Children => null,
                                    Preformatted => Text,
                                    Language => Strings.To_UString (Format)));
   end Add_Preformatted;

   --  ------------------------------
   --  Add a new table in the document with the column styles.
   --  ------------------------------
   procedure Add_Table (Into    : in out Document;
                        Columns : in Column_Array_Style) is
      Table : Node_Type_Access;
   begin
      Table := new Node_Type '(Kind       => N_TABLE,
                               Len        => Columns'Length,
                               Children   => null,
                               Parent     => Into.Current,
                               Columns    => Columns);
      Append (Into, Table);
      Into.Current := Table;
   end Add_Table;

   --  ------------------------------
   --  Add a new header/body/footer row to the current table.
   --  ------------------------------
   procedure Add_Row (Into : in out Document;
                      Kind  : in Nodes.Row_Kind) is
      Table : Node_Type_Access;
      Row   : Node_Type_Access;
   begin
      --  Identify the current table.
      Table := Into.Current;
      while Table /= null and then Table.Kind /= N_TABLE loop
         Table := Table.Parent;
      end loop;

      --  Create the current table.
      if Table = null then
         Table := new Node_Type '(Kind       => N_TABLE,
                                  Len        => 0,
                                  Children   => null,
                                  Parent     => Into.Current,
                                  others     => <>);
         Append (Into, Table);
      end if;

      --  Add the row.
      case Kind is
         when N_ROW_HEADER =>
            Row := new Node_Type '(Kind       => N_ROW_HEADER,
                                   Len        => 0,
                                   Tag_Start  => TR_TAG,
                                   Children   => null,
                                   Parent     => Table,
                                   others     => <>);

         when N_ROW =>
            Row := new Node_Type '(Kind       => N_ROW,
                                   Len        => 0,
                                   Tag_Start  => TR_TAG,
                                   Children   => null,
                                   Parent     => Table,
                                   others     => <>);

         when N_ROW_FOOTER =>
            Row := new Node_Type '(Kind       => N_ROW_FOOTER,
                                   Len        => 0,
                                   Tag_Start  => TR_TAG,
                                   Children   => null,
                                   Parent     => Table,
                                   others     => <>);

      end case;

      Append (Table, Row);
      Into.Current := Row;
   end Add_Row;

   --  ------------------------------
   --  Add a column to the current table row.  The column is configured with the
   --  given attributes.  The column content is provided through calls to Append.
   --  ------------------------------
   procedure Add_Column (Into : in out Document;
                         Attributes : in out Wiki.Attributes.Attribute_List) is
      Row  : Node_Type_Access;
      Col  : Node_Type_Access;
   begin
      --  Identify the current row.
      Row := Into.Current;
      while Row /= null and then not (Row.Kind in N_ROW | N_ROW_HEADER) loop
         Row := Row.Parent;
      end loop;

      --  Add the new column.
      if Row /= null and then Row.Kind = N_ROW_HEADER then
         Col := new Node_Type '(Kind       => N_COLUMN,
                                Len        => 0,
                                Tag_Start  => TH_TAG,
                                Children   => null,
                                Parent     => Row,
                                Attributes => Attributes);
      else
         Col := new Node_Type '(Kind       => N_COLUMN,
                                Len        => 0,
                                Tag_Start  => TD_TAG,
                                Children   => null,
                                Parent     => Row,
                                Attributes => Attributes);
      end if;
      Append (Row, Col);
      Into.Current := Col;
   end Add_Column;

   --  ------------------------------
   --  Finish the creation of the table.
   --  ------------------------------
   procedure Finish_Table (Into : in out Document) is
      Table : Node_Type_Access;
   begin
      --  Identify the current table.
      Table := Into.Current;
      while Table /= null and then Table.Kind /= N_TABLE loop
         Table := Table.Parent;
      end loop;
      if Table /= null then
         Into.Current := Table.Parent;
      else
         Into.Current := null;
      end if;
   end Finish_Table;

   --  ------------------------------
   --  Finish the creation of the list.
   --  ------------------------------
   procedure Finish_List (Into : in out Document) is
      List : Node_Type_Access;
   begin
      --  Identify the current list.
      List := Into.Current;
      while List /= null and then not (List.Kind in N_LIST_START | N_NUM_LIST_START) loop
         List := List.Parent;
      end loop;
      if List /= null then
         Into.Current := List.Parent;
      else
         Into.Current := null;
      end if;
   end Finish_List;

   --  ------------------------------
   --  Set the current list as loose list (items enclosed by <p>).
   --  ------------------------------
   procedure Set_Loose (Doc : in Document) is
      List : Node_Type_Access := Doc.Current;
   begin
      while List /= null and then not (List.Kind in N_LIST_START | N_NUM_LIST_START) loop
         List := List.Parent;
      end loop;
      if List /= null then
         List.Loose := True;
      end if;
   end Set_Loose;

   --  ------------------------------
   --  Iterate over the nodes of the list and call the <tt>Process</tt> procedure with
   --  each node instance.
   --  ------------------------------
   procedure Iterate (Doc     : in Document;
                      Process : not null access procedure (Node : in Node_Type)) is
   begin
      Iterate (Doc.Nodes, Process);
   end Iterate;

   --  ------------------------------
   --  Returns True if the document is empty.
   --  ------------------------------
   function Is_Empty (Doc : in Document) return Boolean is
   begin
      return Wiki.Nodes.Lists.Is_Empty (Doc.Nodes);
   end Is_Empty;

   --  ------------------------------
   --  Returns True if the document displays the table of contents by itself.
   --  ------------------------------
   function Is_Using_TOC (Doc : in Document) return Boolean is
   begin
      return Doc.Using_TOC;
   end Is_Using_TOC;

   --  ------------------------------
   --  Returns True if the table of contents is visible and must be rendered.
   --  ------------------------------
   function Is_Visible_TOC (Doc : in Document) return Boolean is
   begin
      return Doc.Visible_TOC;
   end Is_Visible_TOC;

   --  ------------------------------
   --  Hide the table of contents.
   --  ------------------------------
   procedure Hide_TOC (Doc : in out Document) is
   begin
      Doc.Visible_TOC := False;
   end Hide_TOC;

   --  ------------------------------
   --  Get the table of content node associated with the document.
   --  ------------------------------
   procedure Get_TOC (Doc : in out Document;
                      TOC : out Wiki.Nodes.Lists.Node_List_Ref) is
   begin
      if Wiki.Nodes.Lists.Is_Empty (Doc.TOC) then
         Append (Doc.TOC, new Node_Type '(Kind => N_TOC, Len => 0, others => <>));
      end if;
      TOC := Doc.TOC;
   end Get_TOC;

   --  ------------------------------
   --  Get the table of content node associated with the document.
   --  ------------------------------
   function Get_TOC (Doc : in Document) return Wiki.Nodes.Lists.Node_List_Ref is
   begin
      return Doc.TOC;
   end Get_TOC;

   --  ------------------------------
   --  Set a link definition.
   --  ------------------------------
   procedure Set_Link (Doc   : in out Document;
                       Name  : in Wiki.Strings.WString;
                       Link  : in Wiki.Strings.WString;
                       Title : in Wiki.Strings.WString) is
      Upper : constant Wiki.Strings.WString
           := Ada.Wide_Wide_Characters.Handling.To_Upper (Name);
   begin
      if not Doc.Links.Contains (Upper) then
         Doc.Links.Include (Upper, Link);
         if Title'Length > 0 then
            Doc.Titles.Include (Upper, Title);
         end if;
      end if;
   end Set_Link;

   --  ------------------------------
   --  Get a link definition.
   --  ------------------------------
   function Get_Link (Doc   : in Document;
                      Label : in Wiki.Strings.WString) return Wiki.Strings.WString is
      Upper : constant Wiki.Strings.WString
           := Ada.Wide_Wide_Characters.Handling.To_Upper (Label);
      Pos : constant Wiki.Strings.Maps.Cursor := Doc.Links.Find (Upper);
   begin
      if Wiki.Strings.Maps.Has_Element (Pos) then
         return Wiki.Strings.Maps.Element (Pos);
      else
         return "";
      end if;
   end Get_Link;

   --  ------------------------------
   --  Get a link definition.
   --  ------------------------------
   function Get_Link_Title (Doc   : in Document;
                            Label : in Wiki.Strings.WString) return Wiki.Strings.WString is
      Upper : constant Wiki.Strings.WString
           := Ada.Wide_Wide_Characters.Handling.To_Upper (Label);
      Pos : constant Wiki.Strings.Maps.Cursor := Doc.Titles.Find (Upper);
   begin
      if Wiki.Strings.Maps.Has_Element (Pos) then
         return Wiki.Strings.Maps.Element (Pos);
      else
         return "";
      end if;
   end Get_Link_Title;

   function Has_Link (Doc   : in Document;
                      Label : in Wiki.Strings.WString) return Boolean is
      Upper : constant Wiki.Strings.WString
        := Ada.Wide_Wide_Characters.Handling.To_Upper (Label);
      Pos : constant Wiki.Strings.Maps.Cursor := Doc.Links.Find (Upper);
   begin
      return Wiki.Strings.Maps.Has_Element (Pos);
   end Has_Link;

end Wiki.Documents;
