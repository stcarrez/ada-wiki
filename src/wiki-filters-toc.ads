-----------------------------------------------------------------------
--  wiki-filters-toc -- Filter for the creation of Table Of Contents
--  Copyright (C) 2016, 2020, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

--  === TOC Filter ===
--  The `TOC_Filter` is a filter used to build the table of contents.
--  It collects the headers with the section level as they are added to the
--  wiki document.  The TOC is built in the wiki document as a separate node
--  and it can be retrieved by using the `Get_TOC` function.  To use
--  the filter, declare an aliased instance:
--
--     TOC : aliased Wiki.Filters.TOC.TOC_Filter;
--
--  and add the filter to the Wiki parser engine:
--
--     Engine.Add_Filter (TOC'Unchecked_Access);
--
package Wiki.Filters.TOC is

   pragma Preelaborate;

   --  ------------------------------
   --  TOC Filter
   --  ------------------------------
   type TOC_Filter is new Filter_Type with private;

   --  Add a text content with the given format to the document.
   overriding
   procedure Add_Text (Filter    : in out TOC_Filter;
                       Document  : in out Wiki.Documents.Document;
                       Text      : in Wiki.Strings.WString;
                       Format    : in Wiki.Format_Map);

   overriding
   procedure Start_Block (Filter   : in out TOC_Filter;
                          Document : in out Wiki.Documents.Document;
                          Kind     : in Wiki.Nodes.Node_Kind;
                          Level    : in Natural);

   overriding
   procedure End_Block (Filter   : in out TOC_Filter;
                        Document : in out Wiki.Documents.Document;
                        Kind     : in Wiki.Nodes.Node_Kind);

private

   type TOC_Filter is new Filter_Type with record
      Header_Level : Integer := -1;
      Header       : Wiki.Strings.UString;
   end record;

end Wiki.Filters.TOC;
