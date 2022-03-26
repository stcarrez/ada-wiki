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

   --  ------------------------------
   --  Parse a textile image.
   --  Example:
   --    !image-link!
   --    !image-link(title)!
   --    !image-path!:http-link
   --  ------------------------------
   procedure Parse_Image (P     : in out Parser;
                          Token : in Wiki.Strings.WChar) is
      Pos       : Natural := P.Line_Pos + 1;
      Last      : Natural;
      C         : Wiki.Strings.WChar;
   begin
      Last := Wiki.Strings.Index (P.Line, '!', Pos);
      if Last = 0 then
         Append (P.Text, Token);
         return;
      end if;

      declare
         Title : Wiki.Strings.UString;
         Link  : Wiki.Strings.UString;
         Title_Pos : Natural;
         Last_Pos  : Natural;
      begin
         Title_Pos := Wiki.Strings.Index (P.Line, '(', Pos);

         if Title_Pos = 0 then
            Last_Pos := Last;
         else
            Last_Pos := Title_Pos;
         end if;

         while Pos < Last_Pos loop
            Wiki.Strings.Append (Link, Wiki.Strings.Element (P.Line, Pos));
            Pos := Pos + 1;
         end loop;

         if Title_Pos > 0 then
            Title_Pos := Title_Pos + 1;
            while Title_Pos < Last - 1 loop
               Wiki.Strings.Append (Title, Wiki.Strings.Element (P.Line, Title_Pos));
               Title_Pos := Title_Pos + 1;
            end loop;
         end if;

         Flush_Text (P);
         P.Line_Pos := Last;
         if not P.Context.Is_Hidden then
            Wiki.Attributes.Clear (P.Attributes);
            Wiki.Attributes.Append (P.Attributes, "src", Link);
            P.Context.Filters.Add_Image (P.Document, Wiki.Strings.To_WString (Title),
                                         P.Attributes);
         end if;
      end;
   end Parse_Image;

   --  ------------------------------
   --  Parse an external link:
   --  Example:
   --    "title":http-link
   --  ------------------------------
   procedure Parse_Link (P     : in out Parser;
                         Token : in Wiki.Strings.WChar) is
      Http : constant Wiki.Strings.WString := ":http";
      Pos  : Natural := P.Line_Pos + 1;
      Last : Natural := Wiki.Strings.Index (P.Line, '"', Pos);
      C    : Wiki.Strings.WChar;
   begin
      if Last = 0 or Last = P.Line_Length then
         Append (P.Text, Token);
         return;
      end if;

      Pos := Last + 1;
      for Expect of Http loop
         C := Wiki.Strings.Element (P.Line, Pos);
         if C /= Expect then
            Append (P.Text, Token);
            return;
         end if;
         Pos := Pos + 1;
      end loop;

      declare
         Title : Wiki.Strings.UString;
         Link  : Wiki.Strings.UString;
      begin
         Pos := P.Line_Pos + 1;
         while Pos < Last loop
            Wiki.Strings.Append (Title, Wiki.Strings.Element (P.Line, Pos));
            Pos := Pos + 1;
         end loop;

         Pos := Last + 2;
         while Pos <= P.Line_Length loop
            C := Wiki.Strings.Element (P.Line, Pos);
            exit when Wiki.Helpers.Is_Space_Or_Newline (C);
            Pos := Pos + 1;
            Wiki.Strings.Append (Link, C);
         end loop;
         Flush_Text (P);
         Wiki.Attributes.Clear (P.Attributes);

         P.Line_Pos := Pos;
         P.Empty_Line := False;
         if not P.Context.Is_Hidden then
            Wiki.Attributes.Append (P.Attributes, NAME_ATTR, Title);
            Wiki.Attributes.Append (P.Attributes, HREF_ATTR, Link);
            P.Context.Filters.Add_Link (P.Document, Wiki.Strings.To_WString (Title), P.Attributes);
         end if;
      end;
   end Parse_Link;

   --  ------------------------------
   --  Parse a bold sequence or a list.
   --  Example:
   --    *name*         (bold)
   --    * item         (list)
   --  ------------------------------
   procedure Parse_Bold_Or_List (P     : in out Parser;
                                 Token : in Wiki.Strings.WChar) is
      C : Wiki.Strings.WChar;
   begin
      Peek (P, C);
      if P.Empty_Line and C = ' ' then
         Put_Back (P, C);
         Common.Parse_List (P, Token);
         return;
      end if;
      Put_Back (P, C);
      Toggle_Format (P, BOLD);
   end Parse_Bold_Or_List;

   procedure Parse_Deleted_Or_Horizontal_Rule (P     : in out Parser;
                                               Token : in Wiki.Strings.WChar) is
      C     : Wiki.Strings.WChar;
      Count : Natural := 1;
      Pos   : Natural := P.Line_Pos + 1;
   begin
      if P.Empty_Line then
         while Pos <= P.Line_Length loop
            C := Wiki.Strings.Element (P.Line, Pos);
            Pos := Pos + 1;
            exit when C /= Token;
            Count := Count + 1;
         end loop;
         if Count >= 4 then
            Flush_Text (P);
            Flush_List (P);
            P.Line_Pos := Pos - 1;
            if not P.Context.Is_Hidden then
               P.Context.Filters.Add_Node (P.Document, Wiki.Nodes.N_HORIZONTAL_RULE);
            end if;
            return;
         end if;
      end if;
      Toggle_Format (P, STRIKEOUT);
   end Parse_Deleted_Or_Horizontal_Rule;

end Wiki.Parsers.Textile;
