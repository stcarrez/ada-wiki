-----------------------------------------------------------------------
--  wiki-parsers-creole -- Creole parser operations
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
                                     Text   : in out Wiki.Buffers.Buffer_Access;
                                     From   : in out Positive);

   --  Parse an image or a pre-formatted section.
   --  Example:
   --    {{url|alt text}}
   --    {{{text}}}
   --    {{{
   --    pre-formatted
   --    }}}
   procedure Parse_Image_Or_Preformatted (Parser : in out Parser_Type;
                                          Text   : in out Wiki.Buffers.Buffer_Access;
                                          From   : in out Positive);

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
                                          Text   : in out Wiki.Buffers.Buffer_Access;
                                          From   : in out Positive) is
      Link  : Wiki.Strings.BString (128);
      Alt   : Wiki.Strings.BString (128);
      Block : Wiki.Buffers.Buffer_Access := Text;
      Pos   : Positive := From;
   begin
      Next (Block, Pos);

      --  Check second marker.
      if Block = null or else Block.Content (Pos) /= '{' then
         Common.Parse_Text (Parser, Text, From);
         return;
      end if;

      --  Check third marker: this is a inline code block.
      Next (Block, Pos);
      if Block /= null and then Block.Content (Pos) = '{' then
         Flush_Text (Parser);
         Parser.Format (CODE) := True;
         Next (Block, Pos);
         Text := Block;
         From := Pos;
         return;
      end if;

      if Block = null then
         Common.Parse_Text (Parser, Text, From, Count => 2);
         return;
      end if;

      Common.Parse_Token (Block, Pos, Parser.Escape_Char, '|', '}', Link);
      if Block /= null and then Block.Content (Pos) = '|' then
         Next (Block, Pos);
         if Block /= null then
            Common.Parse_Token (Block, Pos, Parser.Escape_Char, '}', '}', Alt);
         end if;
      end if;

      if Block /= null and then Block.Content (Pos) = '}' then
         Next (Block, Pos);
      end if;

      if Block = null or else Block.Content (Pos) /= '}' then
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
         Parser.Context.Filters.Add_Image (Parser.Document,
                                           Wiki.Strings.To_WString (Alt),
                                           Parser.Attributes);
      end if;
   end Parse_Image_Or_Preformatted;

   --  ------------------------------
   --  Parse the end of the inline pre-formatted code.
   --  Example:
   --    {{{text}}}
   --  ------------------------------
   procedure Parse_End_Preformatted (Parser : in out Parser_Type;
                                     Text   : in out Wiki.Buffers.Buffer_Access;
                                     From   : in out Positive) is
   begin
      if not Parser.Format (CODE) then
         Common.Parse_Text (Parser, Text, From);
      else
         declare
            Count : constant Natural := Count_Occurence (Text, From, '}');
         begin
            if Count < 3 then
               Common.Parse_Text (Parser, Text, From);
            else
               Flush_Text (Parser);
               Parser.Format (CODE) := False;
               for I in 1 .. 3 loop
                  Next (Text, From);
               end loop;
            end if;
         end;
      end if;
   end Parse_End_Preformatted;

   procedure Parse_Line (Parser : in out Parser_Type;
                         Text   : in Wiki.Buffers.Buffer_Access) is
      Pos    : Natural := 1;
      C      : Wiki.Strings.WChar;
      Count  : Natural;
      Buffer : Wiki.Buffers.Buffer_Access := Text;
   begin
      if Parser.Current_Node = Nodes.N_PREFORMAT then
         Count := Count_Occurence (Buffer, 1, '}');
         if Count /= 3 then
            Common.Append (Parser.Text, Buffer, 1);
            return;
         end if;
         Pop_Block (Parser);
         return;
      end if;

      if Parser.Current_Node = Nodes.N_HEADER then
         Pop_Block (Parser);
      end if;

      C := Buffer.Content (Pos);
      case C is
         when CR | LF =>
            Common.Parse_Paragraph (Parser, Buffer, Pos);
            return;

         when '=' =>
            Common.Parse_Header (Parser, Buffer, Pos, '=');
            if Buffer = null then
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

         when '{' =>
            Common.Parse_Preformatted (Parser, Buffer, Pos, '{');
            if Buffer = null then
               return;
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
            Last : Natural := Buffer.Last;
         begin
            while Pos <= Last loop
               C := Buffer.Content (Pos);
               case C is
                  when '*' =>
                     Parse_Format_Double (Parser, Buffer, Pos, '*', Wiki.STRONG);
                     exit Main when Buffer = null;

                  when '/' =>
                     Parse_Format_Double (Parser, Buffer, Pos, '/', Wiki.EMPHASIS);
                     exit Main when Buffer = null;

                  when '-' =>
                     Parse_Format_Double (Parser, Buffer, Pos, '-', Wiki.STRIKEOUT);
                     exit Main when Buffer = null;

                  when '_' =>
                     Parse_Format_Double (Parser, Buffer, Pos, '/', Wiki.UNDERLINE);
                     exit Main when Buffer = null;

                  when '#' =>
                     Parse_Format_Double (Parser, Buffer, Pos, '#', Wiki.CODE);
                     exit Main when Buffer = null;

                  when ',' =>
                     Parse_Format_Double (Parser, Buffer, Pos, ',', Wiki.SUBSCRIPT);
                     exit Main when Buffer = null;

                  when '^' =>
                     Parse_Format (Parser, Buffer, Pos, '^', Wiki.SUPERSCRIPT);
                     exit Main when Buffer = null;

                  when '[' =>
                     Common.Parse_Link (Parser, Buffer, Pos);
                     exit Main when Buffer = null;

                  when '<' =>
                     Common.Parse_Template (Parser, Buffer, Pos, '<');
                     exit Main when Buffer = null;

                  when '{' =>
                     Parse_Image_Or_Preformatted (Parser, Buffer, Pos);
                     exit Main when Buffer = null;

                  when '}' =>
                     Parse_End_Preformatted (Parser, Buffer, Pos);
                     exit Main when Buffer = null;

                  when CR | LF =>
                     --  if Wiki.Strings.Length (Parser.Text) > 0 then
                        Append (Parser.Text, ' ');
                     --  end if;
                     Pos := Pos + 1;

                  when '\' =>
                     Next (Buffer, Pos);
                     if Buffer = null then
                        Append (Parser.Text, C);
                     elsif Buffer.Content (Pos) /= '\' then
                        Append (Parser.Text, C);
                        Last := Buffer.Last;
                     else
                        Parser.Empty_Line := True;
                        Flush_Text (Parser, Trim => Right);
                        if not Parser.Context.Is_Hidden then
                           Parser.Context.Filters.Add_Node (Parser.Document, Nodes.N_LINE_BREAK);
                        end if;
                        Pos := Pos + 1;
                        Last := Buffer.Last;
                     end if;

                  when '~' =>
                     Next (Buffer, Pos);
                     if Buffer = null then
                        Append (Parser.Text, C);
                     else
                        Append (Parser.Text, Buffer.Content (Pos));
                        Pos := Pos + 1;
                        Last := Buffer.Last;
                     end if;

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

end Wiki.Parsers.Creole;
