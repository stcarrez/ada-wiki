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
with Wiki.Nodes;
package body Wiki.Filters.TOC is

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
