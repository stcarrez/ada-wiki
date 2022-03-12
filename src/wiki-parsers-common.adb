-----------------------------------------------------------------------
--  wiki-parsers-common -- Common operations with several wiki parsers
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
with Ada.Wide_Wide_Characters.Handling;
with Wiki.Nodes;
with Wiki.Helpers;
with Wiki.Parsers.Html;
package body Wiki.Parsers.Common is

   use Wiki.Helpers;
   use Wiki.Nodes;

   Attr_Names_Title_First : constant String_Array (1 .. 4)
     := (NAME_ATTR'Access, HREF_ATTR'Access, LANG_ATTR'Access, TITLE_ATTR'Access);

   Attr_Names_Link_First  : constant String_Array (1 .. 4)
     := (HREF_ATTR'Access, NAME_ATTR'Access, LANG_ATTR'Access, TITLE_ATTR'Access);

   --  ------------------------------
   --  Escape a single character and append it to the wiki text buffer.
   --  ------------------------------
   procedure Parse_Escape (P     : in out Parser;
                           Token : in Wiki.Strings.WChar) is
      pragma Unreferenced (Token);

      C : Wiki.Strings.WChar;
   begin
      if not P.Is_Eof then
         Peek (P, C);
         if C /= CR and C /= LF then
            Append (P.Text, C);
         else
            Put_Back (P, C);
         end if;
      end if;
   end Parse_Escape;

   --  ------------------------------
   --  Parse a wiki heading.  The heading could start with '=' or '!'.
   --  The trailing equals are ignored.
   --  Example:
   --    == Level 2 ==
   --    !!! Level 3
   --  ------------------------------
   procedure Parse_Header (P     : in out Parser;
                           Token : in Wiki.Strings.WChar) is

      procedure Add_Header (Content : in Wiki.Strings.WString);

      C      : Wiki.Strings.WChar;
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

      while Level <= 6 loop
         Peek (P, C);
         exit when C /= Token;
         Level := Level + 1;
      end loop;

      --  Ignore spaces after '=' signs
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

      --  dotclear header is the opposite of Creole for the level.
      Level := Level + P.Header_Offset;
      if Level < 0 then
         Level := -Level;
      end if;
      if Level = 0 then
         Level := 1;
      end if;

      if not P.Context.Is_Hidden then
         Add_Header (P.Text);
      end if;
      P.Empty_Line   := True;
      P.In_Paragraph := False;
      Clear (P.Text);
   end Parse_Header;

   --  ------------------------------
   --  Parse a line break.
   --  Example:
   --     \\    (Creole)
   --     %%%   (Dotclear)
   --  ------------------------------
   procedure Parse_Line_Break (P     : in out Parser;
                               Token : in Wiki.Strings.WChar) is
      C : Wiki.Strings.WChar;
   begin
      Peek (P, C);

      --  Check for escape character
      if Token = P.Escape_Char then
         Parse_Text (P, C);
         return;
      end if;
      if C /= Token then
         Parse_Text (P, Token);
         Put_Back (P, C);
         return;
      end if;

      --  Check for a third '%'.
      if P.Is_Dotclear then
         Peek (P, C);
         if C /= Token then
            Parse_Text (P, Token);
            Parse_Text (P, Token);
            Put_Back (P, C);
            return;
         end if;
      end if;
      P.Empty_Line := True;
      Flush_Text (P);
      if not P.Context.Is_Hidden then
         P.Context.Filters.Add_Node (P.Document, Wiki.Nodes.N_LINE_BREAK);
      end if;
   end Parse_Line_Break;

   --  ------------------------------
   --  Parse a pre-formatted text which starts either by a space or by a sequence
   --  of characters.  Example:
   --    {{{
   --    pre-formatted
   --    }}}
   --    ' pre-formattted'
   --  ------------------------------
   procedure Parse_Preformatted (P     : in out Parser;
                                 Token : in Wiki.Strings.WChar) is
      C          : Wiki.Strings.WChar;
   begin
      if Token /= ' ' then
         Peek (P, C);
         if C /= Token then
            if Token = '`' then
               Parse_Single_Code (P, Token);
            else
               Parse_Text (P, Token);
            end if;
            Put_Back (P, C);
            return;
         end if;
         Peek (P, C);
         if C /= Token then
            Parse_Text (P, Token);
            Parse_Text (P, Token);
            Put_Back (P, C);
            return;
         end if;
         Peek (P, C);

         if Token = '{' then
            if C /= LF and C /= CR then
               Put_Back (P, C);
               Flush_Text (P);
               P.Format (CODE) := True;
               return;
            end if;
            Put_Back (P, C);
         elsif Token = '}' then
            Put_Back (P, C);
            Flush_Text (P);
            P.Format (CODE) := True;
            return;
         else
            Put_Back (P, C);
         end if;

      elsif not P.Empty_Line or else (not P.Is_Dotclear and P.Context.Syntax /= SYNTAX_MEDIA_WIKI)
        or else not P.Document.Is_Root_Node
      then
         Parse_Text (P, Token);
         return;
      end if;

      Parse_Preformatted_Block (P, Token);
   end Parse_Preformatted;

   --  ------------------------------
   --  Parse a pre-formatted text which starts either by a space or by a sequence
   --  of characters.  Example:
   --    {{{
   --    pre-formatted
   --    }}}
   --  ------------------------------
   procedure Parse_Preformatted_Block (P     : in out Parser;
                                       Token : in Wiki.Strings.WChar) is
      use Ada.Wide_Wide_Characters.Handling;
      procedure Add_Preformatted (Content : in Wiki.Strings.WString);

      C          : Wiki.Strings.WChar;
      Stop_Token : Wiki.Strings.WChar;
      Format     : Wiki.Strings.UString;
      Col        : Natural;
      Is_Html    : Boolean := False;

      procedure Add_Preformatted (Content : in Wiki.Strings.WString) is
      begin
         P.Context.Filters.Add_Preformatted (P.Document, Content,
                                             Wiki.Strings.To_WString (Format));
      end Add_Preformatted;

      procedure Add_Preformatted is
         new Wiki.Strings.Wide_Wide_Builders.Get (Add_Preformatted);
      pragma Inline (Add_Preformatted);

   begin
      Flush_Text (P);
      Flush_List (P);
      if Token = ' ' then
         Col := P.Preformat_Column + 1;
         while not P.Is_Eof loop
            Peek (P, C);
            if Col < P.Preformat_Column then
               if C /= ' ' then
                  Put_Back (P, C);
                  exit;
               end if;
               Col := Col + 1;
            elsif C = LF or C = CR then
               Col := 0;
               --  Check for CR + LF and treat it as a single LF.
               if C = CR and then not P.Is_Eof then
                  Peek (P, C);
                  if C = LF then
                     Append (P.Text, C);
                  else
                     Put_Back (P, C);
                     Append (P.Text, LF);
                  end if;
               else
                  Append (P.Text, LF);
               end if;
            else
               Col := Col + 1;
               Append (P.Text, C);
            end if;
         end loop;
      else
         Peek (P, C);
         while not P.Is_Eof and C /= LF and C /= CR loop
            if Strings.Is_Alphanumeric (C) then
               Wiki.Strings.Append (Format, To_Lower (C));
            end if;
            Peek (P, C);
         end loop;
         if Token = '{' then
            Stop_Token := '}';
         else
            Stop_Token := Token;
         end if;
         Flush_List (P);
         Is_Html := Wiki.Strings.To_WString (Format) = "html";
         Col := 0;
         while not P.Is_Eof loop
            Peek (P, C);
            if Stop_Token = C and Col = 0 then
               Peek (P, C);
               if C = Stop_Token then
                  Peek (P, C);
                  exit when C = Stop_Token;
               end if;
               Append (P.Text, Stop_Token);
               Col := Col + 1;
            elsif C = CR then
               Col := 0;
               Peek (P, C);
               if C /= LF then
                  Put_Back (P, C);
               end if;
               C := LF;
            elsif C = LF or C = CR then
               Col := 0;
            else
               Col := Col + 1;
            end if;
            if Is_Html and C = '<' then
               Wiki.Parsers.Html.Parse_Element (P);
            else
               Append (P.Text, C);
            end if;
         end loop;
         Skip_End_Of_Line (P);
      end if;
      P.Empty_Line := True;

      if not Is_Html then
         Add_Preformatted (P.Text);
         Clear (P.Text);
         if not P.Context.Is_Hidden then
            P.Context.Filters.Add_Node (P.Document, Wiki.Nodes.N_PARAGRAPH);
         end if;
         P.In_Paragraph := True;
      end if;
   end Parse_Preformatted_Block;

   --  ------------------------------
   --  Parse a template parameter and expand it to the target buffer.
   --  Example:
   --    {{{1}}}                      MediaWiki
   --    <<<1>>>                      Creole extension
   --  ------------------------------
   procedure Expand_Parameter (P     : in out Parser;
                               Into  : in out Wiki.Strings.BString) is
      procedure Expand (Content : in Wiki.Strings.WString);

      C      : Wiki.Strings.WChar;
      Expect : Wiki.Strings.WChar;
      Param  : Wiki.Strings.BString (256);

      procedure Expand (Content : in Wiki.Strings.WString) is
         Name : constant String := Wiki.Strings.To_String (Content);
         Pos  : constant Attributes.Cursor := Wiki.Attributes.Find (P.Context.Variables, Name);
      begin
         if Wiki.Attributes.Has_Element (Pos) then
            Append (Into, Wiki.Attributes.Get_Wide_Value (Pos));
         else
            Append (Into, P.Param_Char);
            Append (Into, P.Param_Char);
            Append (Into, P.Param_Char);
            Append (Into, Content);
            Append (Into, Expect);
            Append (Into, Expect);
            Append (Into, Expect);
         end if;
      end Expand;

      procedure Expand is
         new Wiki.Strings.Wide_Wide_Builders.Get (Expand);

   begin
      Peek (P, C);
      if C /= P.Param_Char then
         Append (Into, P.Param_Char);
         Put_Back (P, C);
         return;
      end if;
      Peek (P, C);
      if C /= P.Param_Char then
         Append (Into, P.Param_Char);
         Append (Into, P.Param_Char);
         Put_Back (P, C);
         return;
      end if;
      if P.Param_Char = '{' then
         Expect := '}';
      else
         Expect := '>';
      end if;

      --  Collect the parameter name or index until we find the end marker.
      loop
         Peek (P, C);
         exit when P.Is_Eof;
         if C = Expect then
            Peek (P, C);
            if C = Expect then
               Peek (P, C);
               exit when C = Expect;
               Append (Param, Expect);
            end if;
            Append (Param, C);
         else
            Append (Param, C);
         end if;
      end loop;

      --  Expand the result.
      Expand (Param);
   end Expand_Parameter;

   --  ------------------------------
   --  Extract a list of parameters separated by the given separator (ex: '|').
   --  ------------------------------
   procedure Parse_Parameters (P          : in out Parser;
                               Separator  : in Wiki.Strings.WChar;
                               Terminator : in Wiki.Strings.WChar;
                               Names      : in String_Array;
                               Max        : in Positive := 200) is
      procedure Add_Parameter (Content : in Wiki.Strings.WString);

      Index : Positive := 1;

      procedure Add_Parameter (Content : in Wiki.Strings.WString) is
         Last : Natural := Content'Last;
      begin
         while Last >= Content'First and then Wiki.Helpers.Is_Space (Content (Last)) loop
            Last := Last - 1;
         end loop;
         if Index <= Names'Last then
            Wiki.Attributes.Append (P.Attributes, Names (Index).all,
                                    Content (Content'First .. Last));
         else
            declare
               Name : constant String := Positive'Image (Index - Names'Length);
            begin
               Wiki.Attributes.Append (P.Attributes, Name (Name'First + 1 .. Name'Last),
                                       Content (Content'First .. Last));
            end;
         end if;
         Index := Index + 1;
      end Add_Parameter;

      procedure Add_Attribute is
         new Wiki.Strings.Wide_Wide_Builders.Get (Add_Parameter);

      C    : Wiki.Strings.WChar;
      Text : Wiki.Strings.BString (256);
   begin
      Wiki.Attributes.Clear (P.Attributes);
      loop
         Peek (P, C);
         if C = P.Escape_Char then
            Peek (P, C);
            Append (Text, C);
         elsif C = Separator and Index <= Max then
            Add_Attribute (Text);
            Clear (Text);
         elsif C = P.Param_Char then
            Expand_Parameter (P, Text);
         elsif C = Terminator or P.Is_Eof then
            Add_Attribute (Text);
            return;
         elsif Length (Text) > 0 or not Wiki.Helpers.Is_Space (C) then
            Append (Text, C);
         end if;
      end loop;
   end Parse_Parameters;

   --  ------------------------------
   --  Parse a link.
   --  Example:
   --    [name]
   --    [url]
   --    [name|url]
   --    [name|url|language]
   --    [name|url|language|title]
   --  MediaWiki
   --    [[link]]
   --    [[link|name]]
   --    [[link|mode|size|center|alt]]
   --    [http://...]
   --    [http://... title]
   --  ------------------------------
   procedure Parse_Link (P     : in out Parser;
                         Token : in Wiki.Strings.WChar) is

      procedure Add_Mediawiki_Image (Link : in Wiki.Strings.WString);

      --  ------------------------------
      --  Extract MediaWiki image attribute and normalize them for the renderer.
      --  Attributes:   alt,  src,   align,   frame
      --  border,frameless,thumb,thumbnail
      --  left,right,center,none
      --  baseline,sup,super,top,text-top,middle,bottom,text-bottom
      --  <width>px, x<height>px, <width>x<height>px
      --  ------------------------------
      procedure Add_Mediawiki_Image (Link : in Wiki.Strings.WString) is
         procedure Collect_Attribute (Name  : in String;
                                      Value : in Wiki.Strings.WString);

         Len  : constant Natural := Wiki.Attributes.Length (P.Attributes);
         Pos  : Natural := 0;
         Attr : Wiki.Attributes.Attribute_List;

         procedure Collect_Attribute (Name  : in String;
                                      Value : in Wiki.Strings.WString) is
            pragma Unreferenced (Name);
         begin
            Pos := Pos + 1;
            if Pos = 1 then
               return;
            end if;
            if Value = "border" or Value = "frameless" or Value = "thumb"
              or Value = "thumbnail"
            then
               Wiki.Attributes.Append (Attr, String '("frame"), Value);

            elsif Value = "left" or Value = "right" or Value = "center" or Value = "none" then
               Wiki.Attributes.Append (Attr, String '("align"), Value);

            elsif Value = "baseline" or Value = "sup" or Value = "super" or Value = "top"
              or Value  = "text-top" or Value = "middle" or Value = "bottom"
              or Value = "text-bottom"
            then
               Wiki.Attributes.Append (Attr, String '("valign"), Value);

            elsif Value'Length > 3 and then Value (Value'Last - 1 .. Value'Last) = "px" then
               Wiki.Attributes.Append (Attr, String '("size"), Value);

            else
               Wiki.Attributes.Append (Attr, String '("alt"), Value);
               if Pos = Len then
                  P.Context.Filters.Add_Image (P.Document, Value, Attr);
               end if;
               return;
            end if;
            if Pos = Len then
               P.Context.Filters.Add_Image (P.Document, Link, Attr);
            end if;
         end Collect_Attribute;

      begin
         Wiki.Attributes.Append (Attr, String '("src"), Link);
         Wiki.Attributes.Iterate (P.Attributes, Collect_Attribute'Access);
      end Add_Mediawiki_Image;

      C              : Wiki.Strings.WChar;
      Separator      : Wiki.Strings.WChar := '|';
      Double_Bracket : Boolean := P.Link_Double_Bracket;
      Max_Count      : Positive := 200;
   begin
      --  If links have the form '[[link]]', check the second bracket.
      if Double_Bracket then
         Peek (P, C);
         if C /= Token then
            if P.Context.Syntax /= SYNTAX_MEDIA_WIKI and C /= 'h' then
               Append (P.Text, Token);
               Put_Back (P, C);
               return;
            end if;
            Put_Back (P, C);
            Separator := ' ';
            Double_Bracket := False;
            Max_Count := 1;
         end if;
      end if;
      if P.Link_No_Space then
         Peek (P, C);
         if C = ' ' then
            Append (P.Text, Token);
            Put_Back (P, C);
            return;
         end if;
         Put_Back (P, C);
      end if;
      Flush_Text (P);

      Wiki.Attributes.Clear (P.Attributes);
      if P.Link_Title_First then
         Parse_Parameters (P, Separator, ']', Attr_Names_Title_First, Max_Count);
      else
         Parse_Parameters (P, Separator, ']', Attr_Names_Link_First, Max_Count);
      end if;

      if Double_Bracket then
         Peek (P, C);
         if C /= ']' then
            Put_Back (P, C);
         end if;
      end if;
      P.Empty_Line := False;
      if not P.Context.Is_Hidden then
         declare
            Link : constant Strings.WString := Attributes.Get_Attribute (P.Attributes, HREF_ATTR);
            Name : constant Strings.WString := Attributes.Get_Attribute (P.Attributes, NAME_ATTR);
            Pos  : constant Natural := P.Is_Image (Link);
         begin
            if Pos > 0 then
               Add_Mediawiki_Image (Link (Pos .. Link'Last));
            else
               if P.Link_Title_First and Link'Length = 0 then
                  Wiki.Attributes.Append (P.Attributes, HREF_ATTR, Name);
               end if;
               if not P.Link_Title_First and Name'Length = 0 then
                  P.Context.Filters.Add_Link (P.Document, Link, P.Attributes);
               else
                  P.Context.Filters.Add_Link (P.Document, Name, P.Attributes);
               end if;
            end if;
         end;
      end if;
      Peek (P, C);
      if not P.Is_Eof then
         if C = CR or C = LF then
            Append (P.Text, C);
         end if;
         Put_Back (P, C);
      end if;
   end Parse_Link;

   procedure Parse_List (P     : in out Parser;
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
         exit when C /= Token;
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
   end Parse_List;

end Wiki.Parsers.Common;
