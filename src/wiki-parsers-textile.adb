-----------------------------------------------------------------------
--  wiki-parsers-textile -- Textile parser operations
--  Copyright (C) 2022 Stephane Carrez
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
with Wiki.Nodes;
with Wiki.Helpers;
with Wiki.Parsers.Common;
package body Wiki.Parsers.Textile is

   use Wiki.Helpers;
   use Wiki.Buffers;
   use type Wiki.Nodes.Node_Kind;

   function Get_Header_Level (Text : in Wiki.Strings.WString) return Natural;

   procedure Parse_Header (Parser  : in out Parser_Type;
                           Text    : in out Wiki.Buffers.Buffer_Access;
                           From    : in out Positive;
                           Level   : in Positive);

   procedure Parse_Image (Parser  : in out Parser_Type;
                          Text    : in out Wiki.Buffers.Buffer_Access;
                          From    : in out Positive);

   procedure Parse_Link (Parser  : in out Parser_Type;
                         Text    : in out Wiki.Buffers.Buffer_Access;
                         From    : in out Positive);

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
                           Text    : in out Wiki.Buffers.Buffer_Access;
                           From    : in out Positive;
                           Level   : in Positive) is
   begin
      Flush_Text (Parser, Trim => Right);
      Pop_Block (Parser);
      Parser.Header_Level := Level;
      Push_Block (Parser, Nodes.N_HEADER);

      declare
         Pos    : Natural := From;
         Buffer : Wiki.Buffers.Buffer_Access := Text;
      begin
         while Buffer /= null loop
            declare
               Last : constant Natural := Buffer.Last;
            begin
               while Pos <= Last loop
                  if not Wiki.Helpers.Is_Space_Or_Newline (Buffer.Content (Pos)) then
                     Common.Append (Parser.Text, Buffer, Pos);
                     Text := null;
                     From := 1;
                     return;
                  end if;
                  Pos := Pos + 1;
               end loop;
            end;
            Buffer := Buffer.Next_Block;
            Pos := 1;
         end loop;
      end;
   end Parse_Header;

   --  ------------------------------
   --  Parse a textile image.
   --  Example:
   --    !image-link!
   --    !image-link(title)!
   --    !image-path!:http-link
   --  ------------------------------
   procedure Parse_Image (Parser  : in out Parser_Type;
                          Text    : in out Wiki.Buffers.Buffer_Access;
                          From    : in out Positive) is
      Block : Wiki.Buffers.Buffer_Access := Text;
      Pos   : Positive := From;
   begin
      Next (Block, Pos);
      Find (Block, Pos, '!');
      if Block = null then
         Common.Parse_Text (Parser, Text, From);
         return;
      end if;

      declare
         Link      : Wiki.Strings.BString (128);
         Title     : Wiki.Strings.BString (128);
         C         : Wiki.Strings.WChar := Wiki.Html_Parser.NUL;
      begin
         Block := Text;
         Pos := From;
         Next (Block, Pos);
         while Block /= null loop
            C := Block.Content (Pos);
            exit when C in '(' | '!';
            Append (Link, C);
            Next (Block, Pos);
         end loop;

         if C = '(' then
            Next (Block, Pos);
            while Block /= null loop
               C := Block.Content (Pos);
               exit when C in ')' | '!';
               Append (Title, C);
               Next (Block, Pos);
            end loop;
            if C /= ')' then
               Common.Parse_Text (Parser, Text, From);
               return;
            end if;
            Next (Block, Pos);
         end if;
         Next (Block, Pos);
         Text := Block;
         From := Pos;

         Flush_Text (Parser);
         if not Parser.Context.Is_Hidden then
            Wiki.Attributes.Clear (Parser.Attributes);
            Wiki.Attributes.Append (Parser.Attributes, "src", Link);
            Parser.Context.Filters.Add_Image (Parser.Document,
                                              Strings.To_WString (Title),
                                              Parser.Attributes);
         end if;
      end;
   end Parse_Image;

   --  ------------------------------
   --  Parse an external link:
   --  Example:
   --    "title":http-link
   --  ------------------------------
   procedure Parse_Link (Parser  : in out Parser_Type;
                         Text    : in out Wiki.Buffers.Buffer_Access;
                         From    : in out Positive) is
      Block : Wiki.Buffers.Buffer_Access := Text;
      Pos   : Positive := From;
      Http : constant Wiki.Strings.WString := ":http";
   begin
      Next (Block, Pos);
      Find (Block, Pos, '"');
      if Block = null then
         Common.Parse_Text (Parser, Text, From);
         return;
      end if;

      Next (Block, Pos);
      for Expect of Http loop
         if Block = null or else Block.Content (Pos) /= Expect then
            Common.Parse_Text (Parser, Text, From);
            return;
         end if;
         Next (Block, Pos);
      end loop;

      declare
         Title : Wiki.Strings.BString (128);
         Link  : Wiki.Strings.BString (128);
         C     : Wiki.Strings.WChar;
      begin
         Block := Text;
         Pos := From;
         Next (Block, Pos);
         while Block.Content (Pos) /= '"' loop
            Append (Title, Block.Content (Pos));
            Next (Block, Pos);
         end loop;

         --  Skip the ":
         Next (Block, Pos);
         Next (Block, Pos);

         while Block /= null loop
            C := Block.Content (Pos);
            exit when Wiki.Helpers.Is_Space_Or_Newline (C);
            Append (Link, C);
            Next (Block, Pos);
         end loop;

         Text := Block;
         From := Pos;

         Flush_Text (Parser);
         Wiki.Attributes.Clear (Parser.Attributes);

         Parser.Empty_Line := False;
         if not Parser.Context.Is_Hidden then
            Wiki.Attributes.Append (Parser.Attributes, NAME_ATTR, Title);
            Wiki.Attributes.Append (Parser.Attributes, HREF_ATTR, Link);
            Parser.Context.Filters.Add_Link (Parser.Document,
                                             Wiki.Strings.To_WString (Title),
                                             Parser.Attributes);
         end if;
      end;
   end Parse_Link;

   procedure Parse_Line (Parser : in out Parser_Type;
                         Text   : in Wiki.Buffers.Buffer_Access) is
      Pos    : Natural := 1;
      C      : Wiki.Strings.WChar;
      Buffer : Wiki.Buffers.Buffer_Access := Text;
      Level  : Natural;
   begin
      --  Feed the HTML parser if there are some pending state.
      if not Wiki.Html_Parser.Is_Empty (Parser.Html) then
         Common.Parse_Html_Element (Parser, Buffer, Pos, Start => False);
         if Buffer = null then
            return;
         end if;
      end if;

      if Parser.Pre_Tag_Counter > 0 then
         Common.Parse_Html_Preformatted (Parser, Buffer, Pos);
         if Buffer = null then
            return;
         end if;
      end if;

      if Parser.Current_Node = Nodes.N_HEADER then
         Pop_Block (Parser);
      end if;

      C := Buffer.Content (Pos);
      case C is
         when CR | LF =>
            Common.Parse_Paragraph (Parser, Buffer, Pos);
            return;

         when 'h' =>
            Level := Get_Header_Level (Buffer.Content (1 .. Buffer.Last));
            if Level > 0 and then Level <= 6 then
               Pos := 4;
               Parse_Header (Parser, Buffer, Pos, Level);
               return;
            end if;

         when '-' =>
            Common.Parse_Horizontal_Rule (Parser, Buffer, Pos, '-');
            if Buffer = null then
               return;
            end if;

         when '*' | '#' =>
            if Common.Is_List (Buffer, Pos) then
               Common.Parse_List (Parser, Buffer, Pos);
            end if;

         when ' ' =>
            Parser.Preformat_Indent := 1;
            Parser.Preformat_Fence := ' ';
            Parser.Preformat_Fcount := 1;
            Flush_Text (Parser, Trim => Right);
            Pop_Block (Parser);
            Push_Block (Parser, Nodes.N_PREFORMAT);
            Common.Append (Parser.Text, Buffer, Pos + 1);
            return;

         when ';' =>
            Common.Parse_Definition (Parser, Buffer, Pos);
            return;

         when ':' =>
            if Parser.Current_Node = Nodes.N_DEFINITION then
               Next (Buffer, Pos);
               Buffers.Skip_Spaces (Buffer, Pos, Level);
            end if;

         when others =>
            if Parser.Current_Node /= Nodes.N_PARAGRAPH then
               Pop_List (Parser);
               Push_Block (Parser, Nodes.N_PARAGRAPH);
            end if;

      end case;

      Main :
      while Buffer /= null loop
         declare
            Last : constant Natural := Buffer.Last;
         begin
            while Pos <= Last loop
               C := Buffer.Content (Pos);
               case C is
                  when '*' =>
                     if Parser.Format (CODE) then
                        Append (Parser.Text, C);
                        Pos := Pos + 1;
                     else
                        Parse_Format (Parser, Buffer, Pos, '*', Wiki.BOLD);
                        exit Main when Buffer = null;
                     end if;

                  when '@' =>
                     Parse_Format (Parser, Buffer, Pos, '@', Wiki.CODE);
                     exit Main when Buffer = null;

                  when '_' =>
                     if Parser.Format (CODE) then
                        Append (Parser.Text, C);
                        Pos := Pos + 1;
                     else
                        Parse_Format (Parser, Buffer, Pos, '_', Wiki.EMPHASIS);
                        exit Main when Buffer = null;
                     end if;

                  when '^' =>
                     Parse_Format (Parser, Buffer, Pos, '^', Wiki.SUPERSCRIPT);
                     exit Main when Buffer = null;

                  when '[' =>
                     Common.Parse_Link (Parser, Buffer, Pos);
                     exit Main when Buffer = null;

                  when '?' =>
                     Parse_Format_Double (Parser, Buffer, Pos, '?', Wiki.CITE);
                     exit Main when Buffer = null;

                  when '{' =>
                     Common.Parse_Template (Parser, Buffer, Pos, '{');
                     exit Main when Buffer = null;

                  when '"' =>
                     Parse_Link (Parser, Buffer, Pos);
                     exit Main when Buffer = null;

                  when '!' =>
                     Parse_Image (Parser, Buffer, Pos);
                     exit Main when Buffer = null;

                  when CR | LF =>
                     Append (Parser.Text, ' ');
                     Pos := Pos + 1;

                  when '<' =>
                     Common.Parse_Html_Element (Parser, Buffer, Pos, Start => True);
                     exit Main when Buffer = null;

                  when '&' =>
                     Common.Parse_Entity (Parser, Buffer, Pos);
                     exit Main when Buffer = null;

                  when others =>
                     Append (Parser.Text, C);
                     Pos := Pos + 1;

               end case;
            end loop;
         end;
         Buffer := Buffer.Next_Block;
         Pos := 1;
      end loop Main;
   end Parse_Line;

end Wiki.Parsers.Textile;
