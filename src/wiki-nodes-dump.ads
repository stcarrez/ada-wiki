-----------------------------------------------------------------------
--  wiki-nodes-dump -- Dump the wiki nodes
--  Copyright (C) 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

generic
   with procedure Write (Indent : in Positive;
                         Line   : in Wiki.Strings.WString);
procedure Wiki.Nodes.Dump (Node : in Wiki.Nodes.Node_Type);
