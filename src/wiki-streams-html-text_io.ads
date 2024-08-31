-----------------------------------------------------------------------
--  wiki-streams-html-text_io -- Wiki HTML output stream on Ada Text_IO
--  Copyright (C) 2016, 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Wiki.Streams.Text_IO;
with Wiki.Streams.Html.Stream;

package Wiki.Streams.Html.Text_IO is
   new Wiki.Streams.Html.Stream (Wiki.Streams.Text_IO.File_Output_Stream);
