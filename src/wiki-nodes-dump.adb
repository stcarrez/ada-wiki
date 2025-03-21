-----------------------------------------------------------------------
--  wiki-nodes-dump -- Dump the wiki nodes
--  Copyright (C) 2022, 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Wiki.Nodes.Lists;
with Wiki.Strings;
with Wiki.Attributes;
procedure Wiki.Nodes.Dump (Node : in Wiki.Nodes.Node_Type) is

   procedure Dump_Node (Node : in Wiki.Nodes.Node_Type);

   Level : Positive := 1;

   procedure Dump_Node (Node : in Wiki.Nodes.Node_Type) is
      procedure Print_Length (Len : Natural);
      procedure Print (Name  : in String;
                       Value : in Wiki.Strings.WString);

      Kind   : constant Wiki.Strings.WString := Node_Kind'Wide_Wide_Image (Node.Kind);
      Result : Wiki.Strings.BString (256);

      procedure Print (Name  : in String;
                       Value : in Wiki.Strings.WString) is
      begin
         Strings.Append_String (Result, " ");
         Strings.Append_String (Result, Strings.To_WString (Name));
         Strings.Append_String (Result, "=");
         Strings.Append_String (Result, Value);
      end Print;

      procedure Print_Length (Len : Natural) is
         S : constant Wide_Wide_String := Natural'Wide_Wide_Image (Len);
      begin
         Strings.Append_String (Result, " (");
         Strings.Append_String (Result, S (S'First + 1 .. S'Last));
         Strings.Append_String (Result, ")");
      end Print_Length;

   begin
      if Node.Kind = N_TAG_START then
         Strings.Append_String (Result, Html_Tag'Wide_Wide_Image (Node.Tag_Start));
      else
         Strings.Append_String (Result, Kind);
      end if;
      Print_Length (Node.Len);
      case Node.Kind is
         when N_HEADER | N_BLOCKQUOTE | N_INDENT | N_TOC_ENTRY | N_NUM_LIST_START
          | N_LIST_START | N_DEFINITION | N_DEFINITION_TERM =>
            Strings.Append_String (Result, Natural'Wide_Wide_Image (Node.Level));
            Strings.Append_String (Result, " ");
            Write (Level, Wiki.Strings.To_WString (Result));
            if Node.Children /= null then
               Level := Level + 1;
               Lists.Iterate (Node.Children, Dump_Node'Access);
               Level := Level - 1;
            end if;
            return;

         when N_TEXT =>
            for Format in Format_Type'Range loop
               if Node.Format (Format) then
                  Strings.Append_String (Result, " ");
                  Strings.Append_String (Result, Format_Type'Wide_Wide_Image (Format));
               end if;
            end loop;
            Strings.Append_String (Result, " ");
            Strings.Append_String (Result, Node.Text);

         when N_IMAGE | N_IMAGE_REF | N_LINK | N_LINK_REF | N_QUOTE | N_LIST_ITEM =>
            Strings.Append_String (Result, " ");
            if Node.Kind /= N_LIST_ITEM then
               Strings.Append_String (Result, Node.Title);
               Attributes.Iterate (Node.Link_Attr, Print'Access);
               Write (Level, Wiki.Strings.To_WString (Result));
            else
               Write (Level, Wiki.Strings.To_WString (Result));
            end if;
            if Node.Children /= null then
               Level := Level + 1;
               Lists.Iterate (Node.Children, Dump_Node'Access);
               Level := Level - 1;
            end if;
            return;

         when N_TAG_START | N_ROW | N_ROW_HEADER | N_ROW_FOOTER | N_COLUMN =>
            Attributes.Iterate (Node.Attributes, Print'Access);
            Write (Level, Wiki.Strings.To_WString (Result));
            if Node.Children /= null then
               Level := Level + 1;
               Lists.Iterate (Node.Children, Dump_Node'Access);
               Level := Level - 1;
            end if;
            return;

         when N_TABLE =>
            Write (Level, Wiki.Strings.To_WString (Result));
            if Node.Children /= null then
               Level := Level + 1;
               Lists.Iterate (Node.Children, Dump_Node'Access);
               Level := Level - 1;
            end if;
            return;

         when N_PREFORMAT =>
            Strings.Append_String (Result, " ");
            Strings.Append_String (Result, Strings.To_WString (Node.Language));
            Strings.Append_String (Result, " ");
            Strings.Append_String (Result, Node.Preformatted);

         when others =>
            null;

      end case;
      Write (Level, Wiki.Strings.To_WString (Result));
   end Dump_Node;

begin
   Dump_Node (Node);
end Wiki.Nodes.Dump;
