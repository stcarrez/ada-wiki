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
package body Wiki.Parsers.Creole is

   use Wiki.Helpers;

   --  ------------------------------
   --  Parse an image or a pre-formatted section.
   --  Example:
   --    {{url|alt text}}
   --    {{{text}}}
   --    {{{
   --    pre-formatted
   --    }}}
   --  ------------------------------
   procedure Parse_Creole_Image_Or_Preformatted (P     : in out Parser;
                                                 Token : in Wiki.Strings.WChar) is

      --  Parse a image component
      procedure Parse_Image_Token (Into : in out Wiki.Strings.UString);

      Link       : Wiki.Strings.UString;
      Alt        : Wiki.Strings.UString;
      C          : Wiki.Strings.WChar;

      procedure Parse_Image_Token (Into : in out Wiki.Strings.UString) is
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
      end Parse_Image_Token;

   begin
      Peek (P, C);
      if C /= Token then
         Append (P.Text, Token);
         Put_Back (P, C);
         return;
      end if;

      Peek (P, C);
      if C = Token then
         Peek (P, C);
         if C /= LF and C /= CR then
            Put_Back (P, C);
            P.Format (CODE) := True;
            return;
         end if;

         Put_Back (P, C);
         Common.Parse_Preformatted_Block (P, Token);
         return;
      end if;
      Put_Back (P, C);

      Parse_Image_Token (Link);
      if C = '|' then
         Parse_Image_Token (Alt);
      end if;
      if C /= '}' then
         Put_Back (P, C);
      end if;
      Flush_Text (P);
      if not P.Context.Is_Hidden then
         Wiki.Attributes.Clear (P.Attributes);
         Wiki.Attributes.Append (P.Attributes, "src", Link);
         P.Context.Filters.Add_Image (P.Document, Wiki.Strings.To_WString (Alt), P.Attributes);
      end if;
      Peek (P, C);
      if C /= '}' then
         Put_Back (P, C);
      end if;
   end Parse_Creole_Image_Or_Preformatted;

   procedure Parse_List_Or_Bold (P     : in out Parser;
                                 Token : in Wiki.Strings.WChar) is
      C     : Wiki.Strings.WChar;
      Level : Natural := 1;
   begin
      if not P.Empty_Line then
         Common.Parse_Double_Bold (P, Token);
         return;
      end if;
      loop
         Peek (P, C);
         exit when C /= '#' and C /= '*';
         Level := Level + 1;
      end loop;
      Flush_Text (P);
      if not P.Context.Is_Hidden then
         P.Context.Filters.Add_List_Item (P.Document, Level, Token = '#');
      end if;

      --  Ignore the first white space after the list item.
      if C /= ' ' and C /= HT then
         Put_Back (P, C);
      end if;
   end Parse_List_Or_Bold;

end Wiki.Parsers.Creole;
