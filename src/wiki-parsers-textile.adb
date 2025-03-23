-----------------------------------------------------------------------
--  wiki-parsers-textile -- Textile parser operations
--  Copyright (C) 2022 - 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Wiki.Nodes;
with Wiki.Helpers;
with Wiki.Parsers.Common;
package body Wiki.Parsers.Textile is

   use Wiki.Helpers;
   use Wiki.Buffers;
   use type Wiki.Nodes.Node_Kind;

   function Get_Header_Level (Text : in Wiki.Strings.WString) return Natural;

   procedure Parse_Header (Parser  : in out Parser_Type;
                           Text    : in out Wiki.Buffers.Cursor;
                           Level   : in Positive);

   procedure Parse_Image (Parser  : in out Parser_Type;
                          Text    : in out Wiki.Buffers.Cursor);

   procedure Parse_Link (Parser  : in out Parser_Type;
                         Text    : in out Wiki.Buffers.Cursor);

   function Get_Header_Level (Text : in Wiki.Strings.WString) return Natural is
   begin
      if Text'Length <= 4 or else Text (Text'First) /= 'h' then
         return 0;
      end if;
      if Text (Text'First + 2) /= '.' or else not Is_Space (Text (Text'First + 3)) then
         return 0;
      end if;
      case Text (Text'First + 1) is
         when '1' =>
            return 1;

         when '2' =>
            return 2;

         when '3' =>
            return 3;

         when '4' =>
            return 4;

         when '5' =>
            return 5;

         when others =>
            return 0;
      end case;
   end Get_Header_Level;

   --  ------------------------------
   --  Parse a textile wiki heading in the form 'h<N>.'.
   --  Example:
   --    h1. Level 1
   --    h2. Level 2
   --  ------------------------------
   procedure Parse_Header (Parser  : in out Parser_Type;
                           Text    : in out Wiki.Buffers.Cursor;
                           Level   : in Positive) is
      Pos : Wiki.Buffers.Cursor := Text;
   begin
      Flush_Text (Parser, Trim => Right);
      Pop_Block (Parser);
      Parser.Header_Level := Level;
      Push_Block (Parser, Nodes.N_HEADER, Level);

      while Buffers.Is_Valid (Pos) loop
         if not Wiki.Helpers.Is_Space_Or_Newline (Buffers.Char_At (Pos)) then
            Common.Append (Parser.Text, Pos);
            Text := (null, 1);
            return;
         end if;
         Buffers.Next (Pos);
      end loop;
   end Parse_Header;

   --  ------------------------------
   --  Parse a textile image.
   --  Example:
   --    !image-link!
   --    !image-link(title)!
   --    !image-path!:http-link
   --  ------------------------------
   procedure Parse_Image (Parser  : in out Parser_Type;
                          Text    : in out Wiki.Buffers.Cursor) is
      Pos   : Wiki.Buffers.Cursor := Text;
   begin
      Next (Pos);
      Find (Pos.Block, Pos.Pos, '!');
      if not Buffers.Is_Valid (Pos) then
         Common.Parse_Text (Parser, Text);
         return;
      end if;

      declare
         Link      : Wiki.Strings.BString (128);
         Title     : Wiki.Strings.BString (128);
         C         : Wiki.Strings.WChar := Wiki.Html_Parser.NUL;
      begin
         Pos := Text;
         Next (Pos);
         while Buffers.Is_Valid (Pos) loop
            C := Buffers.Char_At (Pos);
            exit when C in '(' | '!';
            Append (Link, C);
            Next (Pos);
         end loop;

         if C = '(' then
            Next (Pos);
            while Buffers.Is_Valid (Pos) loop
               C := Buffers.Char_At (Pos);
               exit when C in ')' | '!';
               Append (Title, C);
               Next (Pos);
            end loop;
            if C /= ')' then
               Common.Parse_Text (Parser, Text);
               return;
            end if;
            Next (Pos);
         end if;
         Next (Pos);
         Text := Pos;

         Flush_Text (Parser);
         if not Parser.Context.Is_Hidden then
            Wiki.Attributes.Clear (Parser.Attributes);
            Wiki.Attributes.Append (Parser.Attributes, "src", Link);
            Parser.Context.Filters.Add_Image (Parser.Document,
                                              Strings.To_WString (Title),
                                              Parser.Attributes, False);
         end if;
      end;
   end Parse_Image;

   --  ------------------------------
   --  Parse an external link:
   --  Example:
   --    "title":http-link
   --  ------------------------------
   procedure Parse_Link (Parser  : in out Parser_Type;
                         Text    : in out Wiki.Buffers.Cursor) is
      Pos  : Wiki.Buffers.Cursor := Text;
      Http : constant Wiki.Strings.WString := ":http";
   begin
      Next (Pos);
      Find (Pos.Block, Pos.Pos, '"');
      if not Buffers.Is_Valid (Pos) then
         Common.Parse_Text (Parser, Text);
         return;
      end if;

      Next (Pos);
      for Expect of Http loop
         if not Buffers.Is_Valid (Pos) or else Buffers.Char_At (Pos) /= Expect then
            Common.Parse_Text (Parser, Text);
            return;
         end if;
         Next (Pos);
      end loop;

      declare
         Title : Wiki.Strings.BString (128);
         Link  : Wiki.Strings.BString (128);
         C     : Wiki.Strings.WChar;
      begin
         Pos := Text;
         Next (Pos);
         while Buffers.Is_Valid (Pos) and then Buffers.Char_At (Pos) /= '"' loop
            Append (Title, Buffers.Char_At (Pos));
            Next (Pos);
         end loop;

         --  Skip the ":
         Next (Pos);
         Next (Pos);

         while Buffers.Is_Valid (Pos) loop
            C := Buffers.Char_At (Pos);
            exit when Wiki.Helpers.Is_Space_Or_Newline (C);
            Append (Link, C);
            Next (Pos);
         end loop;

         Text := Pos;

         Flush_Text (Parser);
         Wiki.Attributes.Clear (Parser.Attributes);

         Parser.Empty_Line := False;
         if not Parser.Context.Is_Hidden then
            Wiki.Attributes.Append (Parser.Attributes, NAME_ATTR, Title);
            Wiki.Attributes.Append (Parser.Attributes, HREF_ATTR, Link);
            Parser.Context.Filters.Add_Link (Parser.Document,
                                             Wiki.Strings.To_WString (Title),
                                             Parser.Attributes, False);
         end if;
      end;
   end Parse_Link;

   procedure Parse_Line (Parser : in out Parser_Type;
                         Text   : in Wiki.Buffers.Cursor) is
      Pos    : Wiki.Buffers.Cursor := Text;
      C      : Wiki.Strings.WChar;
      Level  : Natural;
   begin
      --  Feed the HTML parser if there are some pending state.
      if not Wiki.Html_Parser.Is_Empty (Parser.Html) then
         Common.Parse_Html_Element (Parser, Pos, Start => False);
         if not Buffers.Is_Valid (Pos) then
            return;
         end if;
      end if;

      if Parser.Pre_Tag_Counter > 0 then
         Common.Parse_Html_Preformatted (Parser, Pos);
         if not Buffers.Is_Valid (Pos) then
            return;
         end if;
      end if;

      if Parser.Current_Node = Nodes.N_HEADER then
         Pop_Block (Parser);
      end if;

      C := Buffers.Char_At (Pos);
      case C is
         when CR | LF =>
            Common.Parse_Paragraph (Parser, Pos);
            return;

         when 'h' =>
            Level := Get_Header_Level (Pos.Block.Content (1 .. Pos.Block.Last));
            if Level > 0 and then Level <= 6 then
               Pos.Pos := 4;
               Parse_Header (Parser, Pos, Level);
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

         when ';' =>
            Common.Parse_Definition (Parser, Pos, True);

         when ':' =>
            if Parser.Current_Node = Nodes.N_DEFINITION_TERM then
               Common.Parse_Definition (Parser, Pos, False);
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
               if Parser.Format (CODE) then
                  Append (Parser.Text, C);
                  Buffers.Next (Pos);
               else
                  Parse_Format (Parser, Pos, '*', Wiki.BOLD);
               end if;

            when '@' =>
               Parse_Format (Parser, Pos, '@', Wiki.CODE);

            when '_' =>
               if Parser.Format (CODE) then
                  Append (Parser.Text, C);
                  Buffers.Next (Pos);
               else
                  Parse_Format (Parser, Pos, '_', Wiki.EMPHASIS);
               end if;

            when '^' =>
               Parse_Format (Parser, Pos, '^', Wiki.SUPERSCRIPT);

            when '[' =>
               Common.Parse_Link (Parser, Pos);

            when '?' =>
               Parse_Format_Double (Parser, Pos, '?', Wiki.CITE);

            when '{' =>
               Common.Parse_Template (Parser, Pos, '{');

            when '"' =>
               Parse_Link (Parser, Pos);

            when '!' =>
               Parse_Image (Parser, Pos);

            when CR | LF =>
               Append (Parser.Text, ' ');
               Buffers.Next (Pos);

            when '<' =>
               Common.Parse_Html_Element (Parser, Pos, Start => True);

            when '&' =>
               Common.Parse_Entity (Parser, Pos);

            when ':' =>
               if Parser.Current_Node = Nodes.N_DEFINITION_TERM then
                  Common.Parse_Definition (Parser, Pos, False);
               else
                  Append (Parser.Text, C);
                  Buffers.Next (Pos);
               end if;

            when others =>
               Append (Parser.Text, C);
               Buffers.Next (Pos);

         end case;
      end loop;
   end Parse_Line;

end Wiki.Parsers.Textile;
