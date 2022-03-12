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
package body Wiki.Parsers.Textile is

   use Wiki.Helpers;
   use Wiki.Nodes;

   --  ------------------------------
   --  Parse a textile wiki heading in the form 'h<N>.'.
   --  Example:
   --    h1. Level 1
   --    h2. Level 2
   --  ------------------------------
   procedure Parse_Header (P     : in out Parser;
                           Token : in Wiki.Strings.WChar) is

      procedure Add_Header (Content : in Wiki.Strings.WString);

      C      : Wiki.Strings.WChar;
      C2     : Wiki.Strings.WChar;
      Level  : Integer := 1;

      procedure Add_Header (Content : in Wiki.Strings.WString) is
         Last         : Natural := Content'Last;
         Ignore_Token : Boolean := True;
         Seen_Token   : Boolean := False;
      begin
         --  Remove the spaces and '=' at end of header string.
         while Last > Content'First loop
            if Content (Last) = Token then
               exit when not Ignore_Token;
               Seen_Token := True;
            elsif Content (Last) = ' ' or Content (Last) = HT then
               Ignore_Token := not Seen_Token;
            else
               exit;
            end if;
            Last := Last - 1;
         end loop;
         P.Context.Filters.Add_Header (P.Document, Content (Content'First .. Last), Level);
      end Add_Header;

      procedure Add_Header is
         new Wiki.Strings.Wide_Wide_Builders.Get (Add_Header);

   begin
      if not P.Empty_Line then
         Parse_Text (P, Token);
         return;
      end if;

      Peek (P, C);
      case C is
         when '1' =>
            Level := 1;

         when '2' =>
            Level := 2;

         when '3' =>
            Level := 3;

         when '4' =>
            Level := 4;

         when '5' =>
            Level := 5;

         when others =>
            Parse_Text (P, Token);
            Parse_Text (P, C);
            return;

      end case;

      Peek (P, C2);
      if C2 /= '.' then
         Parse_Text (P, Token);
         Parse_Text (P, C);
         Parse_Text (P, C2);
         return;
      end if;

      --  Ignore spaces after the hN. sequence
      Peek (P, C);
      while C = ' ' or C = HT loop
         Peek (P, C);
      end loop;
      Flush_Text (P);
      Flush_List (P);

      loop
         Append (P.Text, C);
         Peek (P, C);
         exit when C = LF or C = CR;
      end loop;

      if not P.Context.Is_Hidden then
         Add_Header (P.Text);
      end if;
      P.Empty_Line   := True;
      P.In_Paragraph := False;
      Clear (P.Text);
   end Parse_Header;

   --  Parse a textile image.
   --  Example:
   --    !image-link!
   --    !image-link(title)!
   --    !image-path|:http-link
   procedure Parse_Image (P     : in out Parser;
                          Token : in Wiki.Strings.WChar) is
   begin
      null;
   end Parse_Image;

   --  Parse an external link:
   --  Example:
   --    "title":http-link
   procedure Parse_Link (P     : in out Parser;
                         Token : in Wiki.Strings.WChar) is
   begin
      null;
   end Parse_Link;

   --  Parse an italic or bold sequence or a list.
   --  Example:
   --    *name*         (italic)
   --    **name**       (bold)
   --    * item         (list)
   procedure Parse_Bold_Or_List (P     : in out Parser;
                                 Token : in Wiki.Strings.WChar) is
   begin
      null;
   end Parse_Bold_Or_List;

   --  Parse a markdown table/column.
   --  Example:
   --    | col1 | col2 | ... | colN |
   procedure Parse_Table (P     : in out Parser;
                          Token : in Wiki.Strings.WChar) is
   begin
      null;
   end Parse_Table;

   procedure Parse_Deleted_Or_Horizontal_Rule (P     : in out Parser;
                                               Token : in Wiki.Strings.WChar) is
   begin
      null;
   end Parse_Deleted_Or_Horizontal_Rule;

end Wiki.Parsers.Textile;
