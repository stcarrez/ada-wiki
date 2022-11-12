-----------------------------------------------------------------------
--  wiki-parsers-dotclear -- Dotclear parser operations
--  Copyright (C) 2011 - 2022 Stephane Carrez
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
package body Wiki.Parsers.Dotclear is

   use Wiki.Helpers;
   use Wiki.Nodes;
   use Wiki.Strings;
   use Wiki.Buffers;

   --  Parse an image.
   --  Example:
   --    ((url|alt text))
   --    ((url|alt text|position))
   --    ((url|alt text|position||description))
   procedure Parse_Image (Parser  : in out Parser_Type;
                          Text    : in out Wiki.Buffers.Buffer_Access;
                          From    : in out Positive);

   procedure Parse_Preformatted (Parser : in out Parser_Type;
                                 Text   : in out Wiki.Buffers.Buffer_Access;
                                 From   : in out Positive);

   procedure Parse_Preformatted (Parser : in out Parser_Type;
                                 Text   : in out Wiki.Buffers.Buffer_Access;
                                 From   : in out Positive) is
      Count : constant Natural := Count_Occurence (Text, From, '/');
   begin
      if Count /= 3 then
         return;
      end if;

      --  Extract the format either 'Ada' or '[Ada]'
      declare
         Pos    : Natural := Count + 1;
         Buffer : Wiki.Buffers.Buffer_Access := Text;
         Space_Count : Natural;
      begin
         Wiki.Strings.Clear (Parser.Preformat_Format);
         Buffers.Skip_Spaces (Buffer, Pos, Space_Count);
         if Buffer /= null and then Pos <= Buffer.Last and then Buffer.Content (Pos) = '[' then
            Next (Buffer, Pos);
            Common.Parse_Token (Buffer, Pos, Parser.Escape_Char, ']', ']',
                                Parser.Preformat_Format);
            if Buffer /= null then
               Next (Buffer, Pos);
            end if;
         else
            Common.Parse_Token (Buffer, Pos, Parser.Escape_Char, CR, LF,
                                Parser.Preformat_Format);
         end if;
         if Buffer /= null then
            Buffers.Skip_Spaces (Buffer, Pos, Space_Count);
         end if;
         Text := Buffer;
         From := Pos;
      end;

      Parser.Preformat_Indent := 0;
      Parser.Preformat_Fence := ' ';
      Parser.Preformat_Fcount := 0;
      Flush_Text (Parser, Trim => Right);
      Pop_Block (Parser);
      Push_Block (Parser, N_PREFORMAT);
   end Parse_Preformatted;

   --  ------------------------------
   --  Parse an image.
   --  Example:
   --    ((url|alt text))
   --    ((url|alt text|position))
   --    ((url|alt text|position||description))
   --  ------------------------------
   procedure Parse_Image (Parser  : in out Parser_Type;
                          Text    : in out Wiki.Buffers.Buffer_Access;
                          From    : in out Positive) is
      procedure Append_Position (Position : in Wiki.Strings.WString);

      procedure Append_Position (Position : in Wiki.Strings.WString) is
      begin
         if Position in "L" | "G" then
            Wiki.Attributes.Append (Parser.Attributes, String '("align"), "left");
         elsif Position in "R" | "D" then
            Wiki.Attributes.Append (Parser.Attributes, String '("align"), "right");
         elsif Position = "C" then
            Wiki.Attributes.Append (Parser.Attributes, String '("align"), "center");
         end if;
      end Append_Position;

      procedure Append_Position is
         new Wiki.Strings.Wide_Wide_Builders.Get (Append_Position);

      Link     : Wiki.Strings.BString (128);
      Alt      : Wiki.Strings.BString (128);
      Position : Wiki.Strings.BString (128);
      Desc     : Wiki.Strings.BString (128);
      Block    : Wiki.Buffers.Buffer_Access := Text;
      Pos      : Positive := From;
   begin
      Next (Block, Pos);
      if Block = null or else Block.Content (Pos) /= '(' then
         Common.Parse_Text (Parser, Text, From);
         return;
      end if;

      Next (Block, Pos);
      if Block = null then
         Common.Parse_Text (Parser, Text, From, Count => 2);
         return;
      end if;

      Common.Parse_Token (Block, Pos, Parser.Escape_Char, '|', ')', Link);
      if Block /= null and then Block.Content (Pos) = '|' then
         Next (Block, Pos);
         if Block /= null then
            Common.Parse_Token (Block, Pos, Parser.Escape_Char, '|', ')', Alt);
         end if;
         if Block /= null and then Block.Content (Pos) = '|' then
            Next (Block, Pos);
            if Block /= null then
               Common.Parse_Token (Block, Pos, Parser.Escape_Char, '|', ')', Position);
            end if;
            if Block /= null and then Block.Content (Pos) = '|' then
               Next (Block, Pos);
               if Block /= null then
                  Common.Parse_Token (Block, Pos, Parser.Escape_Char, '|', ')', Desc);
               end if;
            end if;
         end if;
      end if;

      --  Check for the first ')'.
      if Block /= null and then Block.Content (Pos) = ')' then
         Next (Block, Pos);
      end if;

      --  Check for the second ')', abort the image and emit the '((' if the '))' is missing.
      if Block = null or else Block.Content (Pos) /= ')' then
         Common.Parse_Text (Parser, Text, From, Count => 2);
         return;
      end if;
      Next (Block, Pos);
      Text := Block;
      From := Pos;

      Flush_Text (Parser);
      if not Parser.Context.Is_Hidden then
         Wiki.Attributes.Clear (Parser.Attributes);
         Wiki.Attributes.Append (Parser.Attributes, "src", Link);
         Append_Position (Position);
         Wiki.Attributes.Append (Parser.Attributes, "longdesc", Desc);
         Parser.Context.Filters.Add_Image (Parser.Document,
                                           Strings.To_WString (Alt),
                                           Parser.Attributes);
      end if;
   end Parse_Image;

   procedure Parse_Line (Parser : in out Parser_Type;
                         Text   : in Wiki.Buffers.Buffer_Access) is
      Pos    : Natural := 1;
      C      : Wiki.Strings.WChar;
      Count  : Natural;
      Buffer : Wiki.Buffers.Buffer_Access := Text;
   begin
      if Parser.In_Blockquote then
         Count := Count_Occurence (Buffer, 1, '>');
         if Count = 0 then
            loop
               Pop_Block (Parser);
               exit when Parser.Current_Node = Nodes.N_NONE;
            end loop;
         else
            Buffers.Next (Buffer, Pos, Count);
            Buffers.Skip_Optional_Space (Buffer, Pos);
            if Buffer = null then
               return;
            end if;
            Push_Block (Parser, Nodes.N_BLOCKQUOTE, Count);
         end if;
      end if;

      if Parser.Current_Node = N_PREFORMAT then
         if Parser.Preformat_Fcount = 0 then
            Count := Count_Occurence (Buffer, 1, '/');
            if Count /= 3 then
               Common.Append (Parser.Text, Buffer, 1);
               return;
            end if;
            Pop_Block (Parser);
            return;
         end if;
         if Buffer.Content (Pos) = ' ' then
            Common.Append (Parser.Text, Buffer, Pos + 1);
            return;
         end if;
         Pop_Block (Parser);
      end if;

      if Parser.Current_Node = N_HEADER then
         Pop_Block (Parser);
      end if;

      C := Buffer.Content (Pos);
      if C = '>' then
         Count := Count_Occurence (Buffer, Pos, '>');
         Push_Block (Parser, Nodes.N_BLOCKQUOTE, Count);
         Buffers.Next (Buffer, Pos, Count);
         Buffers.Skip_Optional_Space (Buffer, Pos);
         if Buffer = null then
            return;
         end if;
         C := Buffer.Content (Pos);
      end if;

      case C is
         when CR | LF =>
            Common.Parse_Paragraph (Parser, Buffer, Pos);
            return;

         when '!' =>
            Common.Parse_Header (Parser, Buffer, Pos, '!');
            if Buffer = null then
               return;
            end if;

         when '/' =>
            Parse_Preformatted (Parser, Buffer, Pos);
            if Buffer = null then
               return;
            end if;

         when ' ' =>
            Parser.Preformat_Indent := 1;
            Parser.Preformat_Fence := ' ';
            Parser.Preformat_Fcount := 1;
            Flush_Text (Parser, Trim => Right);
            if Parser.Current_Node /= N_BLOCKQUOTE then
               Pop_Block (Parser);
            end if;
            Push_Block (Parser, N_PREFORMAT);
            Common.Append (Parser.Text, Buffer, Pos + 1);
            return;

         when '-' =>
            Common.Parse_Horizontal_Rule (Parser, Buffer, Pos, '-');
            if Buffer = null then
               return;
            end if;

         when '*' | '#' =>
            Common.Parse_List (Parser, Buffer, Pos);

         when others =>
            if Parser.Current_Node not in N_PARAGRAPH | N_BLOCKQUOTE then
               Pop_List (Parser);
               Push_Block (Parser, N_PARAGRAPH);
            end if;

      end case;

      Main :
      while Buffer /= null loop
         while Pos <= Buffer.Last loop
            C := Buffer.Content (Pos);
            case C is
               when '_' =>
                  Parse_Format_Double (Parser, Buffer, Pos, '_', Wiki.BOLD);
                  exit Main when Buffer = null;

               when ''' =>
                  Parse_Format_Double (Parser, Buffer, Pos, ''', Wiki.ITALIC);
                  exit Main when Buffer = null;

               when '-' =>
                  Parse_Format_Double (Parser, Buffer, Pos, '-', Wiki.STRIKEOUT);
                  exit Main when Buffer = null;

               when '+' =>
                  Parse_Format_Double (Parser, Buffer, Pos, '+', Wiki.INS);
                  exit Main when Buffer = null;

               when ',' =>
                  Parse_Format_Double (Parser, Buffer, Pos, ',', Wiki.SUBSCRIPT);
                  exit Main when Buffer = null;

               when '@' =>
                  Parse_Format_Double (Parser, Buffer, Pos, '@', Wiki.CODE);
                  exit Main when Buffer = null;

               when '^' =>
                  Parse_Format (Parser, Buffer, Pos, '^', Wiki.SUPERSCRIPT);
                  exit Main when Buffer = null;

               when '{' =>
                  Common.Parse_Quote (Parser, Buffer, Pos, '{');
                  exit Main when Buffer = null;

               when '(' =>
                  Parse_Image (Parser, Buffer, Pos);
                  exit Main when Buffer = null;

               when '[' =>
                  Common.Parse_Link (Parser, Buffer, Pos);
                  exit Main when Buffer = null;

               when '<' =>
                  Common.Parse_Template (Parser, Buffer, Pos, '<');
                  exit Main when Buffer = null;

               when '%' =>
                  Count := Count_Occurence (Buffer, Pos, '%');
                  if Count >= 3 then
                     Parser.Empty_Line := True;
                     Flush_Text (Parser, Trim => Right);
                     if not Parser.Context.Is_Hidden then
                        Parser.Context.Filters.Add_Node (Parser.Document, Nodes.N_LINE_BREAK);
                     end if;

                     --  Skip 3 '%' characters.
                     for I in 1 .. 3 loop
                        Next (Buffer, Pos);
                     end loop;
                     if Buffer /= null and then Helpers.Is_Newline (Buffer.Content (Pos)) then
                        Next (Buffer, Pos);
                     end if;
                     exit Main when Buffer = null;
                  else
                     Append (Parser.Text, C);
                     Pos := Pos + 1;
                  end if;

               when CR | LF =>
                  Append (Parser.Text, ' ');
                  Pos := Pos + 1;

               when '\' =>
                  Next (Buffer, Pos);
                  if Buffer = null then
                     Append (Parser.Text, C);
                  else
                     Append (Parser.Text, Buffer.Content (Pos));
                     Pos := Pos + 1;
                  end if;

               when others =>
                  Append (Parser.Text, C);
                  Pos := Pos + 1;

            end case;
         end loop;
         Buffer := Buffer.Next_Block;
         Pos := 1;
      end loop Main;
   end Parse_Line;

end Wiki.Parsers.Dotclear;
