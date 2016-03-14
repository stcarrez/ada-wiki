-----------------------------------------------------------------------
--  wiki-filters-toc -- Filter for the creation of Table Of Contents
--  Copyright (C) 2016 Stephane Carrez
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
with Wiki.Nodes;
package body Wiki.Filters.TOC is

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
      while First <= Text'Last loop
         Pos := Index (Text (First .. Text'Last), "__TOC__");
         if Pos > 0 then
            Filter_Type (Filter).Add_Text (Document, Text (First .. Pos - 1), Format);
            Filter.Add_Node (Document, Wiki.Nodes.N_TOC_DISPLAY);
            First := Pos + 7;
         else
            Filter_Type (Filter).Add_Text (Document, Text (First .. Text'Last), Format);
            exit;
         end if;
      end loop;
   end Add_Text;

   --  ------------------------------
   --  Add a section header with the given level in the document.
   --  ------------------------------
   overriding
   procedure Add_Header (Filter    : in out TOC_Filter;
                         Document  : in out Wiki.Documents.Document;
                         Header    : in Wiki.Strings.WString;
                         Level     : in Natural) is
      T : Wiki.Nodes.Node_List_Ref;
   begin
      Document.Get_TOC (T);
      Wiki.Nodes.Append (T, new Wiki.Nodes.Node_Type '(Kind   => Wiki.Nodes.N_TOC_ENTRY,
                                                       Len    => Header'Length,
                                                       Header => Header,
                                                       Level  => Level));
      Filter_Type (Filter).Add_Header (Document, Header, Level);
   end Add_Header;

end Wiki.Filters.TOC;
