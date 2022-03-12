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
package body Wiki.Parsers.Dotclear is

   use Wiki.Helpers;
   use Wiki.Nodes;

   --  ------------------------------
   --  Parse a quote.
   --  Example:
   --    {{name}}
   --    {{name|language}}
   --    {{name|language|url}}
   --  ------------------------------
   procedure Parse_Quote (P     : in out Parser;
                          Token : in Wiki.Strings.WChar) is

      --  Parse a quote component
      procedure Parse_Quote_Token (Into : in out Wiki.Strings.UString);

      Link       : Wiki.Strings.UString;
      Quote      : Wiki.Strings.UString;
      Language   : Wiki.Strings.UString;
      C          : Wiki.Strings.WChar;

      procedure Parse_Quote_Token (Into : in out Wiki.Strings.UString) is
      begin
         loop
            Peek (P, C);
            if C = P.Escape_Char then
               Peek (P, C);
            else
               exit when C = LF or C = CR or C = '}' or C = '|';
            end if;
            Wiki.Strings.Append (Into, C);
         end loop;
      end Parse_Quote_Token;

   begin
      Peek (P, C);
      if C /= Token then
         Append (P.Text, Token);
         Put_Back (P, C);
         return;
      end if;

      Parse_Quote_Token (Quote);
      if C = '|' then
         Parse_Quote_Token (Language);
         if C = '|' then
            Parse_Quote_Token (Link);
         end if;
      end if;
      if C /= '}' then
         Put_Back (P, C);
      end if;
      Flush_Text (P);
      if not P.Context.Is_Hidden then
         Wiki.Attributes.Clear (P.Attributes);
         Wiki.Attributes.Append (P.Attributes, "cite", Link);
         Wiki.Attributes.Append (P.Attributes, "lang", Language);
         P.Context.Filters.Add_Quote (P.Document, Wiki.Strings.To_WString (Quote), P.Attributes);
      end if;
      Peek (P, C);
      if C /= '}' then
         Put_Back (P, C);
      end if;
   end Parse_Quote;

   --  ------------------------------
   --  Parse an image.
   --  Example:
   --    ((url|alt text))
   --    ((url|alt text|position))
   --    ((url|alt text|position||description))
   --  ------------------------------
   procedure Parse_Image (P     : in out Parser;
                          Token : in Wiki.Strings.WChar) is

      --  Parse a image component
      procedure Parse_Image_Token (Into : in out Wiki.Strings.UString);

      use type Wiki.Strings.UString;

      Link       : Wiki.Strings.UString;
      Alt        : Wiki.Strings.UString;
      Position   : Wiki.Strings.UString;
      Desc       : Wiki.Strings.UString;
      C          : Wiki.Strings.WChar;

      procedure Parse_Image_Token (Into : in out Wiki.Strings.UString) is
      begin
         loop
            Peek (P, C);
            if C = P.Escape_Char then
               Peek (P, C);
            else
               exit when C = LF or C = CR or C = ')' or C = '|';
            end if;
            Wiki.Strings.Append (Into, C);
         end loop;
      end Parse_Image_Token;

   begin
      Peek (P, C);
      if C /= Token then
         Append (P.Text, Token);
         Put_Back (P, C);
         return;
      end if;

      Parse_Image_Token (Link);
      if C = '|' then
         Parse_Image_Token (Alt);
         if C = '|' then
            Parse_Image_Token (Position);
            if C = '|' then
               Parse_Image_Token (Desc);
            end if;
         end if;
      end if;
      if C /= ')' then
         Put_Back (P, C);
      end if;
      Flush_Text (P);
      if not P.Context.Is_Hidden then
         Wiki.Attributes.Clear (P.Attributes);
         Wiki.Attributes.Append (P.Attributes, "src", Link);
         if Position = "L" or Position = "G" then
            Wiki.Attributes.Append (P.Attributes, String '("align"), "left");
         elsif Position = "R" or Position = "D" then
            Wiki.Attributes.Append (P.Attributes, String '("align"), "right");
         elsif Position = "C" then
            Wiki.Attributes.Append (P.Attributes, String '("align"), "center");
         end if;
         Wiki.Attributes.Append (P.Attributes, "longdesc", Desc);
         P.Context.Filters.Add_Image (P.Document, Wiki.Strings.To_WString (Alt), P.Attributes);
      end if;
      Peek (P, C);
      if C /= ')' then
         Put_Back (P, C);
      end if;
   end Parse_Image;

   --  ------------------------------
   --  Parse a horizontal rule.
   --  Example:
   --    ---- (dotclear)
   --  ------------------------------
   procedure Parse_Horizontal_Rule (P     : in out Parser;
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
   end Parse_Horizontal_Rule;

end Wiki.Parsers.Dotclear;
