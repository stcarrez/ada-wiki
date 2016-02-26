-----------------------------------------------------------------------
--  wiki-render-text -- Wiki Text renderer
--  Copyright (C) 2011, 2012, 2013, 2015, 2016 Stephane Carrez
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
   --  Emit a new line.
   --  ------------------------------
   procedure New_Line (Document : in out Text_Renderer) is
   begin
      Document.Output.Write (Wiki.Helpers.LF);
      Document.Empty_Line := True;
   end New_Line;

   --  ------------------------------
   --  Add a line break (<br>).
   --  ------------------------------
   procedure Add_Line_Break (Document : in out Text_Renderer) is
   begin
      Document.Output.Write (Wiki.Helpers.LF);
      Document.Empty_Line := True;
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

   --  ------------------------------
   --  Render a list item (<li>).  Close the previous paragraph and list item if any.
   --  The list item will be closed at the next list item, next paragraph or next header.
   --  ------------------------------
   procedure Render_List_Item (Engine   : in out Text_Renderer;
                               Level    : in Positive;
                               Ordered  : in Boolean) is
      pragma Unreferenced (Level, Ordered);
   begin
      if not Engine.Empty_Line then
         Engine.Add_Line_Break;
      end if;
      Engine.Need_Paragraph := False;
      Engine.Open_Paragraph;
   end Render_List_Item;

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
      if Title /= Href and Href'Length /= 0 then
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
      if Title'Length > 0 and Desc'Length > 0 then
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

   --  Render the node instance from the document.
   overriding
   procedure Render (Engine : in out Text_Renderer;
                     Doc    : in Wiki.Documents.Document;
                     Node   : in Wiki.Nodes.Node_Type) is
      use type Wiki.Html_Tag;
      use type Wiki.Nodes.Node_List_Access;
   begin
      case Node.Kind is
         when Wiki.Nodes.N_HEADER =>
            Engine.Close_Paragraph;
            if not Engine.Empty_Line then
               Engine.Add_Line_Break;
            end if;
            Engine.Output.Write (Node.Header);
            Engine.Add_Line_Break;

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

         when Wiki.Nodes.N_INDENT =>
            Engine.Indent_Level := Node.Level;

         when Wiki.Nodes.N_BLOCKQUOTE =>
            Engine.Render_Blockquote (Node.Level);

         when Wiki.Nodes.N_LIST =>
            Engine.Render_List_Item (Node.Level, False);

         when Wiki.Nodes.N_NUM_LIST =>
            Engine.Render_List_Item (Node.Level, True);

         when Wiki.Nodes.N_TEXT =>
            if Engine.Empty_Line and Engine.Indent_Level /= 0 then
               for I in 1 .. Engine.Indent_Level loop
                  Engine.Output.Write (' ');
               end loop;
            end if;
            Engine.Output.Write (Node.Text);
            Engine.Empty_Line := False;

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
