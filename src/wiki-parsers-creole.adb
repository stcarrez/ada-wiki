-----------------------------------------------------------------------
--  wiki-parsers-creole -- Creole parser operations
--  Copyright (C) 2011 - 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Wiki.Helpers;
with Wiki.Parsers.Common;
package body Wiki.Parsers.Creole is

   use Wiki.Helpers;
   use Wiki.Buffers;
   use type Wiki.Nodes.Node_Kind;

   --  Parse the end of the inline pre-formatted code.
   --  Example:
   --    {{{text}}}
   procedure Parse_End_Preformatted (Parser : in out Parser_Type;
                                     Text   : in out Wiki.Buffers.Cursor);

   --  Parse an image or a pre-formatted section.
   --  Example:
   --    {{url|alt text}}
   --    {{{text}}}
   --    {{{
   --    pre-formatted
   --    }}}
   procedure Parse_Image_Or_Preformatted (Parser : in out Parser_Type;
                                          Text   : in out Wiki.Buffers.Cursor);

   --  ------------------------------
   --  Parse an image or a pre-formatted section.
   --  Example:
   --    {{url|alt text}}
   --    {{{text}}}
   --    {{{
   --    pre-formatted
   --    }}}
   --  ------------------------------
   procedure Parse_Image_Or_Preformatted (Parser : in out Parser_Type;
                                          Text   : in out Wiki.Buffers.Cursor) is
      Link  : Wiki.Strings.BString (128);
      Alt   : Wiki.Strings.BString (128);
      Pos   : Wiki.Buffers.Cursor := Text;
   begin
      Next (Pos);

      --  Check second marker.
      if not Buffers.Is_Valid (Pos) or else Buffers.Char_At (Pos) /= '{' then
         Common.Parse_Text (Parser, Text);
         return;
      end if;

      --  Check third marker: this is a inline code block.
      Next (Pos);
      if Buffers.Is_Valid (Pos) and then Buffers.Char_At (Pos) = '{' then
         Flush_Text (Parser);
         Parser.Format (CODE) := True;
         Next (Pos);
         Text := Pos;
         return;
      end if;

      if not Buffers.Is_Valid (Pos) then
         Common.Parse_Text (Parser, Text, Count => 2);
         return;
      end if;

      Common.Parse_Token (Pos.Block, Pos.Pos, Parser.Escape_Char, '|', '}', Link);
      if Buffers.Is_Valid (Pos) and then Buffers.Char_At (Pos) = '|' then
         Next (Pos);
         if Buffers.Is_Valid (Pos) then
            Common.Parse_Token (Pos.Block, Pos.Pos, Parser.Escape_Char, '}', '}', Alt);
         end if;
      end if;

      if Buffers.Is_Valid (Pos) and then Buffers.Char_At (Pos) = '}' then
         Buffers.Next (Pos);
      end if;

      if not Buffers.Is_Valid (Pos) or else Buffers.Char_At (Pos) /= '}' then
         Common.Parse_Text (Parser, Text, Count => 2);
         return;
      end if;
      Next (Pos);
      Text := Pos;

      Flush_Text (Parser);
      if not Parser.Context.Is_Hidden then
         Wiki.Attributes.Clear (Parser.Attributes);
         Wiki.Attributes.Append (Parser.Attributes, "src", Link);
         Parser.Context.Filters.Add_Image (Parser.Document,
                                           Wiki.Strings.To_WString (Alt),
                                           Parser.Attributes, False);
      end if;
   end Parse_Image_Or_Preformatted;

   --  ------------------------------
   --  Parse the end of the inline pre-formatted code.
   --  Example:
   --    {{{text}}}
   --  ------------------------------
   procedure Parse_End_Preformatted (Parser : in out Parser_Type;
                                     Text   : in out Wiki.Buffers.Cursor) is
   begin
      if not Parser.Format (CODE) then
         Common.Parse_Text (Parser, Text);
      else
         declare
            Count : constant Natural := Count_Occurence (Text.Block, Text.Pos, '}');
         begin
            if Count < 3 then
               Common.Parse_Text (Parser, Text);
            else
               Flush_Text (Parser);
               Parser.Format (CODE) := False;
               Buffers.Next (Text, 3);
            end if;
         end;
      end if;
   end Parse_End_Preformatted;

   procedure Parse_Line (Parser : in out Parser_Type;
                         Text   : in Wiki.Buffers.Cursor) is
      Pos    : Wiki.Buffers.Cursor := Text;
      C      : Wiki.Strings.WChar;
      Count  : Natural;
   begin
      if Parser.Current_Node = Nodes.N_PREFORMAT then
         Count := Count_Occurence (Pos.Block, Pos.Pos, '}');
         if Count /= 3 then
            Common.Append (Parser.Text, Pos);
            return;
         end if;
         Pop_Block (Parser);
         return;
      end if;

      if Parser.Current_Node = Nodes.N_HEADER then
         Pop_Block (Parser);
      end if;

      if not Buffers.Is_Valid (Pos) then
         return;
      end if;

      C := Buffers.Char_At (Pos);
      case C is
         when CR | LF =>
            Common.Parse_Paragraph (Parser, Pos);
            return;

         when '=' =>
            Common.Parse_Header (Parser, Pos, '=');
            if not Buffers.Is_Valid (Pos) then
               return;
            end if;

         when '-' =>
            Common.Parse_Horizontal_Rule (Parser, Pos, '-');
            if not Buffers.Is_Valid (Pos) then
               return;
            end if;

         when '*' | '#' =>
            if Common.Is_List (Pos) then
               Common.Parse_List (Parser, Pos);
            end if;

         when '{' =>
            Common.Parse_Preformatted (Parser, Pos, '{');
            if not Buffers.Is_Valid (Pos) then
               return;
            end if;

         when others =>
            if Parser.Current_Node /= Nodes.N_PARAGRAPH then
               Pop_List (Parser);
               Push_Block (Parser, Nodes.N_PARAGRAPH);
            end if;

      end case;

      while Buffers.Is_Valid (Pos) loop
         C := Buffers.Char_At (Pos);
         case C is
            when '*' =>
               Parse_Format_Double (Parser, Pos, '*', Wiki.STRONG);

            when '/' =>
               Parse_Format_Double (Parser, Pos, '/', Wiki.EMPHASIS);

            when '-' =>
               Parse_Format_Double (Parser, Pos, '-', Wiki.STRIKEOUT);

            when '_' =>
               Parse_Format_Double (Parser, Pos, '/', Wiki.UNDERLINE);

            when '#' =>
               Parse_Format_Double (Parser, Pos, '#', Wiki.CODE);

            when ',' =>
               Parse_Format_Double (Parser, Pos, ',', Wiki.SUBSCRIPT);

            when '^' =>
               Parse_Format (Parser, Pos, '^', Wiki.SUPERSCRIPT);

            when '[' =>
               Common.Parse_Link (Parser, Pos);

            when '<' =>
               Common.Parse_Template (Parser, Pos, '<');

            when '{' =>
               Parse_Image_Or_Preformatted (Parser, Pos);

            when '}' =>
               Parse_End_Preformatted (Parser, Pos);

            when CR | LF =>
               Append (Parser.Text, ' ');
               Buffers.Next (Pos);

            when '\' =>
               Buffers.Next (Pos);
               if not Buffers.Is_Valid (Pos) then
                  Append (Parser.Text, C);
               elsif Buffers.Char_At (Pos) /= '\' then
                  Append (Parser.Text, C);
               else
                  Parser.Empty_Line := True;
                  Flush_Text (Parser, Trim => Right);
                  if not Parser.Context.Is_Hidden then
                     Parser.Context.Filters.Add_Node (Parser.Document, Nodes.N_LINE_BREAK);
                  end if;
                  Buffers.Next (Pos);
               end if;

            when '~' =>
               Buffers.Next (Pos);
               if not Buffers.Is_Valid (Pos) then
                  Append (Parser.Text, C);
               else
                  Append (Parser.Text, Buffers.Char_At (Pos));
                  Buffers.Next (Pos);
               end if;

            when others =>
               Append (Parser.Text, C);
               Buffers.Next (Pos);

         end case;
      end loop;
   end Parse_Line;

end Wiki.Parsers.Creole;
