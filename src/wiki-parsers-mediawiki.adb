-----------------------------------------------------------------------
--  wiki-parsers-mediawiki -- Media Wiki parser operations
--  Copyright (C) 2011- 2022 Stephane Carrez
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
package body Wiki.Parsers.MediaWiki is

   use Wiki.Helpers;
   use Wiki.Nodes;

   Attr_Name              : constant String_Array (1 .. 1)
     := (1 => NAME_ATTR'Access);

   --  ------------------------------
   --  Parse an italic, bold or bold + italic sequence.
   --  Example:
   --    ''name''         (italic)
   --    '''name'''       (bold)
   --    '''''name'''''   (bold+italic)
   --  ------------------------------
   procedure Parse_Bold_Italic (P     : in out Parser;
                                Token : in Wiki.Strings.WChar) is
      C     : Wiki.Strings.WChar;
      Count : Natural := 1;
   begin
      loop
         Peek (P, C);
         exit when C /= Token;
         Count := Count + 1;
      end loop;
      if Count > 10 then
         Count := Count mod 10;
         if Count = 0 then
            Put_Back (P, C);
            return;
         end if;
      end if;

      case Count is
         when 1 =>
            Parse_Text (P, Token);

         when 2 =>
            Toggle_Format (P, ITALIC);

         when 3 =>
            Toggle_Format (P, BOLD);

         when 4 =>
            Toggle_Format (P, BOLD);
            Parse_Text (P, Token);

         when 5 =>
            Toggle_Format (P, BOLD);
            Toggle_Format (P, ITALIC);

         when others =>
            null;
      end case;
      Put_Back (P, C);
   end Parse_Bold_Italic;

   --  ------------------------------
   --  Parse a list definition:
   --    ;item 1
   --    : definition 1
   --  ------------------------------
   procedure Parse_Definition (P     : in out Parser;
                               Token : in Wiki.Strings.WChar) is
   begin
      if not P.Empty_Line then
         Parse_Text (P, Token);
         return;
      end if;
      Flush_Text (P);
      Wiki.Attributes.Clear (P.Attributes);
      if not P.Context.Is_Hidden then
         P.Context.Filters.Push_Node (P.Document, Wiki.DD_TAG, P.Attributes);
      end if;
   end Parse_Definition;

   --  ------------------------------
   --  Parse a list definition:
   --    ;item 1
   --    : definition 1
   --  ------------------------------
   procedure Parse_Item (P     : in out Parser;
                         Token : in Wiki.Strings.WChar) is
   begin
      if not P.Empty_Line then
         Parse_Text (P, Token);
         return;
      end if;
      Flush_Text (P);
      Wiki.Attributes.Clear (P.Attributes);
      if not P.Context.Is_Hidden then
         if not P.In_List then
            P.Context.Filters.Push_Node (P.Document, Wiki.DL_TAG, P.Attributes);
         end if;
         P.Context.Filters.Push_Node (P.Document, Wiki.DT_TAG, P.Attributes);
      end if;
      P.In_List := True;
   end Parse_Item;

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

   --  ------------------------------
   --  Parse a template with parameters.
   --  Example:
   --    {{Name|param|...}}           MediaWiki
   --    {{Name|param=value|...}}     MediaWiki
   --    <<Name param=value ...>>     Creole
   --    [{Name param=value ...}]     JSPWiki
   --  ------------------------------
   procedure Parse_Template (P     : in out Parser;
                             Token : in Wiki.Strings.WChar) is
      use type Wiki.Plugins.Wiki_Plugin_Access;

      C      : Wiki.Strings.WChar;
      Expect : Wiki.Strings.WChar;
   begin
      Peek (P, C);
      if C /= Token then
         Append (P.Text, Token);
         Put_Back (P, C);
         return;
      end if;
      Peek (P, C);
      if C = Token then
         Parse_Parameter (P, Token);
         return;
      end if;
      Flush_Text (P);
      Put_Back (P, C);
      if Token = '{' then
         Expect := '}';
      else
         Expect := '>';
      end if;
      Wiki.Attributes.Clear (P.Attributes);
      if P.Context.Syntax = SYNTAX_MEDIA_WIKI then
         Common.Parse_Parameters (P, '|', Expect, Attr_Name);
      else
         Common.Parse_Parameters (P, ' ', Expect, Attr_Name);
      end if;
      Peek (P, C);
      if C /= Expect then
         Put_Back (P, C);
      end if;
      P.Empty_Line := False;
      declare
         use type Wiki.Strings.UString;
         Name    : constant Strings.WString := Attributes.Get_Attribute (P.Attributes, NAME_ATTR);
         Plugin  : constant Wiki.Plugins.Wiki_Plugin_Access := P.Find (Name);
         Context : Wiki.Plugins.Plugin_Context;
         Ctx     : access Wiki.Plugins.Plugin_Context;
      begin
         if Plugin /= null then
            --  Check that we are not including the template recursively.
            Ctx := P.Context'Access;
            while Ctx /= null loop
               if Ctx.Ident = Name then
                  Append (P.Text, "Recursive call to ");
                  Append (P.Text, Name);
                  return;
               end if;
               Ctx := Ctx.Previous;
            end loop;
            Context.Previous := P.Context'Unchecked_Access;
            Context.Factory := P.Context.Factory;
            Context.Syntax  := P.Context.Syntax;
            Context.Variables := P.Attributes;
            Context.Is_Included := True;
            Context.Ident := Wiki.Strings.To_UString (Name);
            Context.Filters.Set_Chain (P.Context.Filters);
            Plugin.Expand (P.Document, P.Attributes, Context);
         end if;
      end;
   end Parse_Template;

   --  ------------------------------
   --  Parse a template parameter and expand it.
   --  Example:
   --    {{{1}}}                      MediaWiki
   --  ------------------------------
   procedure Parse_Parameter (P     : in out Parser;
                              Token : in Wiki.Strings.WChar) is
      C      : Wiki.Strings.WChar;
      Expect : Wiki.Strings.WChar;
      Pos    : Wiki.Attributes.Cursor;
   begin
      if Token = '{' then
         Expect := '}';
      else
         Expect := '>';
      end if;
      Common.Parse_Parameters (P, '|', Expect, Attr_Name);
      Peek (P, C);
      if C = Expect then
         Peek (P, C);
         if C /= Expect then
            Put_Back (P, C);
         end if;
      else
         Put_Back (P, C);
      end if;
      if not P.Context.Is_Hidden then
         declare
            Name : constant String := Attributes.Get_Value (Wiki.Attributes.First (P.Attributes));
         begin
            Pos := Wiki.Attributes.Find (P.Context.Variables, Name);
            if Wiki.Attributes.Has_Element (Pos) then
               Append (P.Text, Wiki.Attributes.Get_Wide_Value (Pos));
            end if;
         end;
      end if;
   end Parse_Parameter;

end Wiki.Parsers.MediaWiki;
