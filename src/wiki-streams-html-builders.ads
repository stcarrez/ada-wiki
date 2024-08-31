-----------------------------------------------------------------------
--  wiki-streams-html-builders -- Wiki writer to a string builder
--  Copyright (C) 2011, 2012, 2013, 2015, 2016, 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Wiki.Streams.Builders;
with Wiki.Streams.Html.Stream;

--  === HTML Output Builder Stream ===
--  The `Html_Output_Builder_Stream` type defines a HTML output stream that collects the
--  HTML into expandable buffers.  Once the complete HTML document is rendered, the content is
--  retrieved either by the `To_String` or the `Iterate` operations.
--
package Wiki.Streams.Html.Builders is
   new Wiki.Streams.Html.Stream (Wiki.Streams.Builders.Output_Builder_Stream);
