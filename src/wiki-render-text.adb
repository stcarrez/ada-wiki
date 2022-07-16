-----------------------------------------------------------------------
--  wiki-render-text -- Wiki Text renderer
--  Copyright (C) 2011, 2012, 2013, 2015, 2016, 2018, 2019, 2022 Stephane Carrez
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
with Util.Strings;
package body Wiki.Render.Text is

   --  ------------------------------
   --  Set the output writer.
   --  ------------------------------
   procedure Set_Output_Stream (Engine : in out Text_Renderer;
                                Stream : in Streams.Output_Stream_Access) is
   begin
      Engine.Output := Stream;
   end Set_Output_Stream;

   --  ------------------------------
   --  Set the no-newline mode to produce a single line text (disabled by default).
   --  ------------------------------
   procedure Set_No_Newline (Engine : in out Text_Renderer;
                             Enable : in Boolean) is
   begin
      Engine.No_Newline := Enable;
   end Set_No_Newline;

   --  ------------------------------
   --  Emit a new line.
   --  ------------------------------
   procedure New_Line (Document : in out Text_Renderer) is
   begin
      if not Document.No_Newline then
         Document.Output.Write (Wiki.Helpers.LF);
      end if;
      Document.Empty_Line := True;
   end New_Line;

   --  ------------------------------
   --  Add a line break (<br>).
   --  ------------------------------
   procedure Add_Line_Break (Document : in out Text_Renderer) is
   begin
      if not Document.No_Newline then
         Document.Output.Write (Wiki.Helpers.LF);
      end if;
      Document.Empty_Line := True;
      Document.Current_Indent := 0;
   end Add_Line_Break;

   --  ------------------------------
   --  Render a blockquote (<blockquote>).  The level indicates the blockquote nested level.
   --  The blockquote must be closed at the next header.
   --  ------------------------------
   procedure Render_Blockquote (Engine : in out Text_Renderer;
                                Level  : in Natural) is
   begin
      Engine.Close_Paragraph;
      for I in 1 .. Level loop
         Engine.Output.Write ("  ");
      end loop;
   end Render_Blockquote;

   procedure Render_List_Start (Engine   : in out Text_Renderer;
                                Tag      : in String;
                                Level    : in Natural) is
      pragma Unreferenced (Tag);
   begin
      if not Engine.Empty_Line then
         Engine.Add_Line_Break;
      end if;
      Engine.Need_Paragraph := False;
      Engine.Open_Paragraph;
      if not Engine.No_Newline then
         Engine.Output.Write (Wiki.Helpers.LF);
      end if;
      Engine.List_Index := Engine.List_Index + 1;
      Engine.List_Levels (Engine.List_Index) := Level;
      Engine.Indent_Level := Engine.Indent_Level + 2;
   end Render_List_Start;

   procedure Render_List_End (Engine   : in out Text_Renderer;
                              Tag      : in String) is
      pragma Unreferenced (Tag);
   begin
      if not Engine.Empty_Line then
         Engine.Add_Line_Break;
      end if;
      Engine.Need_Paragraph := False;
      Engine.Open_Paragraph;
      Engine.List_Index := Engine.List_Index - 1;
      Engine.Indent_Level := Engine.Indent_Level - 2;
   end Render_List_End;

   --  ------------------------------
   --  Render a list item (<li>).  Close the previous paragraph and list item if any.
   --  The list item will be closed at the next list item, next paragraph or next header.
   --  ------------------------------
   procedure Render_List_Item_Start (Engine   : in out Text_Renderer) is
   begin
      if not Engine.Empty_Line then
         Engine.Add_Line_Break;
      end if;
      Engine.Need_Paragraph := False;
      Engine.Open_Paragraph;

      if Engine.List_Levels (Engine.List_Index) > 0 then
         Engine.Render_Paragraph
           (Strings.To_WString (Util.Strings.Image (Engine.List_Levels (Engine.List_Index))));
         Engine.List_Levels (Engine.List_Index) := Engine.List_Levels (Engine.List_Index) + 1;
         Engine.Render_Paragraph (") ");
         Engine.Indent_Level := Engine.Indent_Level + 4;
      else
         Engine.Render_Paragraph ("- ");
         Engine.Indent_Level := Engine.Indent_Level + 2;
      end if;
   end Render_List_Item_Start;

   procedure Render_List_Item_End (Engine   : in out Text_Renderer) is
   begin
      if not Engine.Empty_Line then
         Engine.Add_Line_Break;
      end if;
      Engine.Need_Paragraph := False;
      Engine.Open_Paragraph;
      if Engine.List_Levels (Engine.List_Index) > 0 then
         Engine.Indent_Level := Engine.Indent_Level - 4;
      else
         Engine.Indent_Level := Engine.Indent_Level - 2;
      end if;
   end Render_List_Item_End;

   procedure Close_Paragraph (Document : in out Text_Renderer) is
   begin
      if Document.Has_Paragraph then
         Document.Add_Line_Break;
      end if;
      Document.Has_Paragraph := False;
   end Close_Paragraph;

   procedure Open_Paragraph (Document : in out Text_Renderer) is
   begin
      if Document.Need_Paragraph then
         Document.Has_Paragraph  := True;
         Document.Need_Paragraph := False;
      end if;
   end Open_Paragraph;

   --  ------------------------------
   --  Render a link.
   --  ------------------------------
   procedure Render_Link (Engine : in out Text_Renderer;
                          Title  : in Wiki.Strings.WString;
                          Attr   : in Wiki.Attributes.Attribute_List) is
      Href : constant Wiki.Strings.WString := Wiki.Attributes.Get_Attribute (Attr, "href");
   begin
      Engine.Open_Paragraph;
      if Title'Length /= 0 then
         Engine.Output.Write (Title);
      end if;
      if Title /= Href and then Href'Length /= 0 then
         if Title'Length /= 0 then
            Engine.Output.Write (" (");
         end if;
         Engine.Output.Write (Href);
         if Title'Length /= 0 then
            Engine.Output.Write (")");
         end if;
      end if;
      Engine.Empty_Line := False;
   end Render_Link;

   --  ------------------------------
   --  Render an image.
   --  ------------------------------
   procedure Render_Image (Engine   : in out Text_Renderer;
                           Title    : in Wiki.Strings.WString;
                           Attr     : in Wiki.Attributes.Attribute_List) is
      Desc : constant Wiki.Strings.WString := Wiki.Attributes.Get_Attribute (Attr, "longdesc");
   begin
      Engine.Open_Paragraph;
      if Title'Length > 0 then
         Engine.Output.Write (Title);
      end if;
      if Title'Length > 0 and then Desc'Length > 0 then
         Engine.Output.Write (' ');
      end if;
      if Desc'Length > 0 then
         Engine.Output.Write (Desc);
      end if;
      Engine.Empty_Line := False;
   end Render_Image;

   --  ------------------------------
   --  Render a text block that is pre-formatted.
   --  ------------------------------
   procedure Render_Preformatted (Engine : in out Text_Renderer;
                                  Text   : in Wiki.Strings.WString;
                                  Format : in Wiki.Strings.WString) is
      pragma Unreferenced (Format);
   begin
      Engine.Close_Paragraph;
      Engine.Output.Write (Text);
      Engine.Empty_Line := False;
   end Render_Preformatted;

   --  ------------------------------
   --  Render a text block indenting the text if necessary.
   --  ------------------------------
   procedure Render_Paragraph (Engine : in out Text_Renderer;
                               Text   : in Wiki.Strings.WString) is
   begin
      for C of Text loop
         if C = Helpers.LF then
            Engine.Empty_Line := True;
            Engine.Current_Indent := 0;
            Engine.Output.Write (C);
         else
            while Engine.Current_Indent < Engine.Indent_Level loop
               Engine.Output.Write (' ');
               Engine.Current_Indent := Engine.Current_Indent + 1;
            end loop;

            Engine.Output.Write (C);
            Engine.Empty_Line := False;
            Engine.Current_Indent := Engine.Current_Indent + 1;
            Engine.Has_Paragraph := True;
         end if;
      end loop;
   end Render_Paragraph;

   --  Render the node instance from the document.
   overriding
   procedure Render (Engine : in out Text_Renderer;
                     Doc    : in Wiki.Documents.Document;
                     Node   : in Wiki.Nodes.Node_Type) is
      use type Wiki.Nodes.Node_List_Access;
   begin
      case Node.Kind is
         when Wiki.Nodes.N_HEADER =>
            Engine.Close_Paragraph;
            if not Engine.Empty_Line then
               Engine.Add_Line_Break;
            end if;
            Engine.Render (Doc, Node.Content);
            Engine.Add_Line_Break;
            Engine.Has_Paragraph := False;
            Engine.Empty_Line := False;

         when Wiki.Nodes.N_LINE_BREAK =>
            Engine.Add_Line_Break;

         when Wiki.Nodes.N_HORIZONTAL_RULE =>
            Engine.Close_Paragraph;
            Engine.Output.Write ("---------------------------------------------------------");
            Engine.Add_Line_Break;

         when Wiki.Nodes.N_PARAGRAPH =>
            Engine.Close_Paragraph;
            Engine.Need_Paragraph := True;
            Engine.Add_Line_Break;

         when Wiki.Nodes.N_NEWLINE =>
            if not Engine.No_Newline then
               Engine.Output.Write (Wiki.Helpers.LF);
            else
               Engine.Output.Write (' ');
            end if;

         when Wiki.Nodes.N_INDENT =>
            Engine.Indent_Level := Node.Level;

         when Wiki.Nodes.N_BLOCKQUOTE =>
            Engine.Render_Blockquote (Node.Level);

         when Wiki.Nodes.N_LIST_START =>
            Engine.Render_List_Start ("o", 0);

         when Wiki.Nodes.N_NUM_LIST_START =>
            Engine.Render_List_Start (".", Node.Level);

         when Wiki.Nodes.N_LIST_END =>
            Engine.Render_List_End ("");

         when Wiki.Nodes.N_NUM_LIST_END =>
            Engine.Render_List_End ("");

         when Wiki.Nodes.N_LIST_ITEM =>
            Engine.Render_List_Item_Start;

         when Wiki.Nodes.N_LIST_ITEM_END =>
            Engine.Render_List_Item_End;

         when Wiki.Nodes.N_TEXT =>
            Engine.Render_Paragraph (Node.Text);

         when Wiki.Nodes.N_QUOTE =>
            Engine.Open_Paragraph;
            Engine.Output.Write (Node.Title);
            Engine.Empty_Line := False;

         when Wiki.Nodes.N_LINK =>
            Engine.Render_Link (Node.Title, Node.Link_Attr);

         when Wiki.Nodes.N_IMAGE =>
            Engine.Render_Image (Node.Title, Node.Link_Attr);

         when Wiki.Nodes.N_PREFORMAT =>
            Engine.Render_Preformatted (Node.Preformatted, "");

         when Wiki.Nodes.N_TAG_START =>
            if Node.Children /= null then
               if Node.Tag_Start = Wiki.DT_TAG then
                  Engine.Close_Paragraph;
                  Engine.Indent_Level := 0;
                  Engine.Render (Doc, Node.Children);
                  Engine.Close_Paragraph;
                  Engine.Indent_Level := 0;
               elsif Node.Tag_Start = Wiki.DD_TAG then
                  Engine.Close_Paragraph;
                  Engine.Empty_Line := True;
                  Engine.Indent_Level := 4;
                  Engine.Render (Doc, Node.Children);
                  Engine.Close_Paragraph;
                  Engine.Indent_Level := 0;
               else
                  Engine.Render (Doc, Node.Children);
                  if Node.Tag_Start = Wiki.DL_TAG then
                     Engine.Close_Paragraph;
                     Engine.New_Line;
                  end if;
               end if;
            end if;

         when others =>
            null;

      end case;
   end Render;

   --  ------------------------------
   --  Finish the document after complete wiki text has been parsed.
   --  ------------------------------
   overriding
   procedure Finish (Engine : in out Text_Renderer;
                     Doc    : in Wiki.Documents.Document) is
      pragma Unreferenced (Doc);
   begin
      Engine.Close_Paragraph;
   end Finish;

end Wiki.Render.Text;
