-----------------------------------------------------------------------
--  wiki-filters-toc -- Filter for the creation of Table Of Contents
--  Copyright (C) 2016, 2018, 2020, 2022 Stephane Carrez
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
with Ada.Strings.Wide_Wide_Fixed;
with Wiki.Nodes.Lists;
package body Wiki.Filters.TOC is

   use type Wiki.Nodes.Node_Kind;

   --  ------------------------------
   --  Add a text content with the given format to the document.
   --  ------------------------------
   overriding
   procedure Add_Text (Filter    : in out TOC_Filter;
                       Document  : in out Wiki.Documents.Document;
                       Text      : in Wiki.Strings.WString;
                       Format    : in Wiki.Format_Map) is
      use Ada.Strings.Wide_Wide_Fixed;

      First : Natural := Text'First;
      Pos   : Natural;
   begin
      if Filter.Header_Level >= 0 then
         Wiki.Strings.Append (Filter.Header, Text);
         Filter_Type (Filter).Add_Text (Document, Text, Format);
         return;
      end if;

      while First <= Text'Last loop
         Pos := Index (Text (First .. Text'Last), "__TOC__");
         if Pos > 0 then
            Filter_Type (Filter).Add_Text (Document, Text (First .. Pos - 1), Format);
            Filter.Add_Node (Document, Wiki.Nodes.N_TOC_DISPLAY);
            First := Pos + 7;
         else
            Pos := Index (Text (First .. Text'Last), "__NOTOC__");
            if Pos > 0 then
               Filter_Type (Filter).Add_Text (Document, Text (First .. Pos - 1), Format);
               Document.Hide_TOC;
               First := Pos + 9;
            else
               Filter_Type (Filter).Add_Text (Document, Text (First .. Text'Last), Format);

               exit;
            end if;
         end if;
      end loop;
   end Add_Text;

   --  ------------------------------
   --  Add a section header with the given level in the document.
   --  ------------------------------
   overriding
   procedure Start_Block (Filter   : in out TOC_Filter;
                          Document : in out Wiki.Documents.Document;
                          Kind     : in Wiki.Nodes.Node_Kind;
                          Level    : in Natural) is
   begin
      if Kind = Nodes.N_HEADER then
         Filter.Header_Level := Level;
      end if;
      Filter.Header_Level := Level;

      Filter_Type (Filter).Start_Block (Document, Kind, Level);
   end Start_Block;

   overriding
   procedure End_Block (Filter   : in out TOC_Filter;
                        Document : in out Wiki.Documents.Document;
                        Kind     : in Wiki.Nodes.Node_Kind) is
   begin
      if Kind = Nodes.N_HEADER then
         declare
            T : Wiki.Nodes.Lists.Node_List_Ref;
            Header : constant Wiki.Strings.WString
              := Wiki.Strings.To_WString (Filter.Header);
         begin
            Document.Get_TOC (T);
            Wiki.Nodes.Lists.Append (T,
                                     new Wiki.Nodes.Node_Type
                                       '(Kind   => Wiki.Nodes.N_TOC_ENTRY,
                                         Len    => Header'Length,
                                         Header => Header,
                                         Parent => null,
                                         Toc_Level  => Filter.Header_Level));
            Filter.Header_Level := -1;
            Filter.Header := Wiki.Strings.To_UString ("");
         end;
      end if;
      Filter_Type (Filter).End_Block (Document, Kind);
   end End_Block;

end Wiki.Filters.TOC;
