-----------------------------------------------------------------------
--  wiki-parsers-markdown -- Markdown parser operations
--  Copyright (C) 2016 - 2022 Stephane Carrez
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
package body Wiki.Parsers.Markdown is

   use Wiki.Helpers;
   use Wiki.Nodes;

   --  ------------------------------
   --  Parse a blockquote.
   --  Example:
   --    >>>quote level 3
   --    >>quote level 2
   --    >quote level 1
   --  ------------------------------
   procedure Parse_Blockquote (P     : in out Parser;
                               Token : in Wiki.Strings.WChar) is
      C     : Wiki.Strings.WChar;
      Level : Natural := 1;
   begin
      if not P.Empty_Line then
         Parse_Text (P, Token);
         return;
      end if;
      loop
         Peek (P, C);
         exit when C /= '>';
         Level := Level + 1;
      end loop;
      Flush_Text (P);
      Flush_List (P);
      P.Empty_Line := True;
      P.Quote_Level := Level;
      if not P.Context.Is_Hidden then
         P.Context.Filters.Add_Blockquote (P.Document, Level);
      end if;

      --  Ignore the first white space after the quote character.
      if C /= ' ' and C /= HT then
         Put_Back (P, C);
      end if;
   end Parse_Blockquote;

   --  ------------------------------
   --  Parse a markdown link.
   --  Example:
   --    [title](url)
   --  ------------------------------
   procedure Parse_Markdown_Link (P     : in out Parser;
                                  Token : in Wiki.Strings.WChar) is
      pragma Unreferenced (Token);

      --  Parse a title/link component
      procedure Parse_Link_Token (Into   : in out Wiki.Strings.UString;
                                   Marker : in Wiki.Strings.WChar);

      Link       : Wiki.Strings.UString;
      Title      : Wiki.Strings.UString;
      C          : Wiki.Strings.WChar;

      procedure Parse_Link_Token (Into   : in out Wiki.Strings.UString;
                                   Marker : in Wiki.Strings.WChar) is
      begin
         loop
            Peek (P, C);
            if C = P.Escape_Char then
               Peek (P, C);
            else
               exit when C = LF or C = CR or C = Marker;
            end if;
            Wiki.Strings.Append (Into, C);
         end loop;
      end Parse_Link_Token;

   begin
      Parse_Link_Token (Title, ']');
      Peek (P, C);
      if C /= '(' then
         Append (P.Text, '[');
         Append (P.Text, Wiki.Strings.To_WString (Title));
         Append (P.Text, ']');
         Put_Back (P, C);
         return;
      end if;
      Parse_Link_Token (Link, ')');
      Flush_Text (P);
      if not P.Context.Is_Hidden then
         Wiki.Attributes.Clear (P.Attributes);
         Wiki.Attributes.Append (P.Attributes, HREF_ATTR, Link);
         P.Context.Filters.Add_Link (P.Document, Wiki.Strings.To_WString (Title),
                                     P.Attributes);
      end if;
      P.Empty_Line := False;
   end Parse_Markdown_Link;

   --  ------------------------------
   --  Parse a markdown image.
   --  Example:
   --    ![title](link)
   --  ------------------------------
   procedure Parse_Markdown_Image (P     : in out Parser;
                                   Token : in Wiki.Strings.WChar) is

      --  Parse a image component
      procedure Parse_Image_Token (Into   : in out Wiki.Strings.UString;
                                   Marker : in Wiki.Strings.WChar);

      Link       : Wiki.Strings.UString;
      Title      : Wiki.Strings.UString;
      C          : Wiki.Strings.WChar;

      procedure Parse_Image_Token (Into   : in out Wiki.Strings.UString;
                                   Marker : in Wiki.Strings.WChar) is
      begin
         loop
            Peek (P, C);
            if C = P.Escape_Char then
               Peek (P, C);
            else
               exit when C = LF or C = CR or C = Marker;
            end if;
            Wiki.Strings.Append (Into, C);
         end loop;
      end Parse_Image_Token;

   begin
      Peek (P, C);
      if C /= '[' then
         Append (P.Text, Token);
         Put_Back (P, C);
         return;
      end if;

      Parse_Image_Token (Title, ']');
      Peek (P, C);
      if C = '(' then
         Parse_Image_Token (Link, ')');
      end if;
      Flush_Text (P);
      if not P.Context.Is_Hidden then
         Wiki.Attributes.Clear (P.Attributes);
         Wiki.Attributes.Append (P.Attributes, "src", Link);
         P.Context.Filters.Add_Image (P.Document, Wiki.Strings.To_WString (Title),
                                      P.Attributes);
      end if;
   end Parse_Markdown_Image;

   --  ------------------------------
   --  Parse a line break or escape character for Markdown.
   --  Example:
   --     \
   --     \`
   --  ------------------------------
   procedure Parse_Markdown_Escape (P     : in out Parser;
                                    Token : in Wiki.Strings.WChar) is
      C : Wiki.Strings.WChar;
   begin
      Peek (P, C);

      case C is
         when LF | CR =>
            P.Empty_Line := True;
            Flush_Text (P);
            if not P.Context.Is_Hidden then
               P.Context.Filters.Add_Node (P.Document, Wiki.Nodes.N_LINE_BREAK);
            end if;

         when '\' | '`' | '*' | '_' | '{' | '}'
            | '[' | ']' | '(' | ')' | '#' | '+'
            | '-' | '.' | '!' | '^' =>
            Parse_Text (P, C);

         when others =>
            Parse_Text (P, Token);
            Put_Back (P, C);

      end case;
   end Parse_Markdown_Escape;

   --  ------------------------------
   --  Parse an italic or bold sequence or a list.
   --  Example:
   --    *name*         (italic)
   --    **name**       (bold)
   --    * item         (list)
   --  ------------------------------
   procedure Parse_Markdown_Bold_Italic (P     : in out Parser;
                                         Token : in Wiki.Strings.WChar) is
      C     : Wiki.Strings.WChar;
   begin
      Peek (P, C);
      if Token = '*' and P.Empty_Line and C = ' ' then
         Put_Back (P, C);
         Common.Parse_List (P, Token);
         return;
      end if;
      if C = Token then
         Toggle_Format (P, BOLD);
      else
         Toggle_Format (P, ITALIC);
         Put_Back (P, C);
      end if;
   end Parse_Markdown_Bold_Italic;

   --  ------------------------------
   --  Parse a horizontal rule or a list.
   --  Example:
   --    ---  (horizontal rule)
   --    -    (list)
   --  ------------------------------
   procedure Parse_Markdown_Horizontal_Rule (P     : in out Parser;
                                             Token : in Wiki.Strings.WChar) is
      C     : Wiki.Strings.WChar;
      Count : Natural := 1;
   begin
      loop
         Peek (P, C);
         exit when C /= Token;
         Count := Count + 1;
      end loop;
      if Count >= 4 then
         Flush_Text (P);
         Flush_List (P);
         if not P.Context.Is_Hidden then
            P.Context.Filters.Add_Node (P.Document, Wiki.Nodes.N_HORIZONTAL_RULE);
         end if;
         if C /= LF and C /= CR then
            Put_Back (P, C);
         end if;
      elsif P.Is_Dotclear and Count = 2 then
         Toggle_Format (P, STRIKEOUT);
         Put_Back (P, C);
      else
         for I in 1 .. Count loop
            Append (P.Text, Token);
         end loop;
         Put_Back (P, C);
      end if;
   end Parse_Markdown_Horizontal_Rule;

   --  Parse a markdown table/column.
   --  Example:
   --    | col1 | col2 | ... | colN |
   procedure Parse_Table (P     : in out Parser;
                          Token : in Wiki.Strings.WChar) is
      C : Wiki.Strings.WChar;
   begin
      if not P.In_Table then
         if not P.Empty_Line then
            P.Parse_Text (Token);
            return;
         end if;
         P.In_Table := True;
         P.Context.Filters.Add_Row (P.Document);
         Wiki.Attributes.Clear (P.Attributes);
         P.Context.Filters.Add_Column (P.Document, P.Attributes);
         return;
      end if;
      Flush_Text (P);
      Flush_List (P);
      Peek (P, C);
      if C = CR or C = LF then
         Put_Back (P, C);
         return;
      end if;
      if P.Empty_Line then
         P.Context.Filters.Add_Row (P.Document);
      end if;
      Wiki.Attributes.Clear (P.Attributes);
      P.Context.Filters.Add_Column (P.Document, P.Attributes);
   end Parse_Table;

end Wiki.Parsers.Markdown;
