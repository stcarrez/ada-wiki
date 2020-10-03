-----------------------------------------------------------------------
--  wiki-streams-html-builders -- Wiki writer to a string builder
--  Copyright (C) 2011, 2012, 2013, 2015, 2016, 2020 Stephane Carrez
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
with Wiki.Streams.Builders;
with Wiki.Streams.Html.Stream;

--  === HTML Output Builder Stream ===
--  The `Html_Output_Builder_Stream` type defines a HTML output stream that collects the
--  HTML into expandable buffers.  Once the complete HTML document is rendered, the content is
--  retrieved either by the `To_String` or the `Iterate` operations.
--
package Wiki.Streams.Html.Builders is
   new Wiki.Streams.Html.Stream (Wiki.Streams.Builders.Output_Builder_Stream);
