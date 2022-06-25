-----------------------------------------------------------------------
--  wiki-filters-html -- Wiki HTML filters
--  Copyright (C) 2015, 2016, 2020 Stephane Carrez
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
private with Ada.Containers.Vectors;

--  === HTML Filters ===
--  The `Wiki.Filters.Html` package implements a customizable HTML filter that verifies
--  the HTML content embedded in the Wiki text.  The HTML filter can be customized to indicate
--  the HTML tags that must be accepted or ignored.  By default, the filter accepts all HTML
--  tags except 'script', 'noscript', 'style'.
--
--  * A tag can be `Forbidden` in which case it is not passed to the document.
--    If this tag contains inner HTML elements, they are passed to the document.
--    By default, the `html`, `head`, `meta`, `title`, `script`, `body` are not
--    passed to the document.
--  * A tag can be `Hidden` in which case it is not passed to the document and
--    the inner HTML elements it contains are also silently ignored.
--    By default this is the case for `script`, `noscript` and `style`.
--
--  The HTML filter may be declared and configured as follows:
--
--    F : aliased Wiki.Filters.Html.Html_Filter_Type;
--    ...
--    F.Forbidden (Wiki.Filters.Html.A_TAG);
--
--  With this configuration the HTML links will be ignored by the parser.
--  The following configuration:
--
--    F.Hide (Wiki.Filters.Html.TABLE_TAG);
--
--  will remove the table and its content.
--
--  The filter is added to the Wiki parser filter chain by using the <tt>Add_Filter</tt>
--  operation:
--
--    Engine.Add_Filter (F'Unchecked_Access);
--
--
package Wiki.Filters.Html is

   --  ------------------------------
   --  Filter type
   --  ------------------------------
   type Html_Filter_Type is new Filter_Type with private;

   --  Add a simple node such as N_LINE_BREAK, N_HORIZONTAL_RULE or N_PARAGRAPH to the document.
   overriding
   procedure Add_Node (Filter    : in out Html_Filter_Type;
                       Document  : in out Wiki.Documents.Document;
                       Kind      : in Wiki.Nodes.Simple_Node_Kind);

   --  Add a text content with the given format to the document.
   overriding
   procedure Add_Text (Filter    : in out Html_Filter_Type;
                       Document  : in out Wiki.Documents.Document;
                       Text      : in Wiki.Strings.WString;
                       Format    : in Wiki.Format_Map);

   --  Push a HTML node with the given tag to the document.
   overriding
   procedure Push_Node (Filter     : in out Html_Filter_Type;
                        Document   : in out Wiki.Documents.Document;
                        Tag        : in Wiki.Html_Tag;
                        Attributes : in out Wiki.Attributes.Attribute_List);

   --  Pop a HTML node with the given tag.
   overriding
   procedure Pop_Node (Filter   : in out Html_Filter_Type;
                       Document : in out Wiki.Documents.Document;
                       Tag      : in Wiki.Html_Tag);

   --  Add a link.
   overriding
   procedure Add_Link (Filter     : in out Html_Filter_Type;
                       Document   : in out Wiki.Documents.Document;
                       Name       : in Wiki.Strings.WString;
                       Attributes : in out Wiki.Attributes.Attribute_List);

   --  Add an image.
   overriding
   procedure Add_Image (Filter     : in out Html_Filter_Type;
                        Document   : in out Wiki.Documents.Document;
                        Name       : in Wiki.Strings.WString;
                        Attributes : in out Wiki.Attributes.Attribute_List);

   --  Finish the document after complete wiki text has been parsed.
   overriding
   procedure Finish (Filter   : in out Html_Filter_Type;
                     Document : in out Wiki.Documents.Document);

   --  Mark the HTML tag as being forbidden.
   procedure Forbidden (Filter : in out Html_Filter_Type;
                        Tag    : in Html_Tag);

   --  Mark the HTML tag as being allowed.
   procedure Allowed (Filter : in out Html_Filter_Type;
                      Tag    : in Html_Tag);

   --  Mark the HTML tag as being hidden.  The tag and its inner content including the text
   --  will be removed and not passed to the final document.
   procedure Hide (Filter : in out Html_Filter_Type;
                   Tag    : in Html_Tag);

   --  Mark the HTML tag as being visible.
   procedure Visible (Filter : in out Html_Filter_Type;
                      Tag    : in Html_Tag);

   --  Flush the HTML element that have not yet been closed.
   procedure Flush_Stack (Filter   : in out Html_Filter_Type;
                          Document : in out Wiki.Documents.Document);

private

   use Wiki.Nodes;

   package Tag_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => Html_Tag);

   subtype Tag_Vector is Tag_Vectors.Vector;
   subtype Tag_Cursor is Tag_Vectors.Cursor;

   type Html_Filter_Type is new Filter_Type with record
      Allowed    : Tag_Boolean_Array := (UNKNOWN_TAG   => False,
                                         SCRIPT_TAG    => False,
                                         ROOT_HTML_TAG => False,
                                         HEAD_TAG      => False,
                                         BODY_TAG      => False,
                                         META_TAG      => False,
                                         TITLE_TAG     => False,
                                         others        => True);

      Hidden     : Tag_Boolean_Array := (UNKNOWN_TAG  => False,
                                         SCRIPT_TAG   => True,
                                         STYLE_TAG    => True,
                                         NOSCRIPT_TAG => True,
                                         others       => False);
      Stack      : Tag_Vector;
      Hide_Level : Natural := 0;
   end record;

end Wiki.Filters.Html;
