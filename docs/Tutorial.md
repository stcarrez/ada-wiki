## Parsing and rendering example

### Parsing a Wiki Document

To render a Wiki text you will first need to parse the Wiki text and produce a Wiki document instance.
For this you will need to declare the Wiki document instance and the Wiki parser instance:

```
 with Wiki.Documents;
 with Wiki.Parsers;
 ...
    Doc      : Wiki.Documents.Document;
    Engine   : Wiki.Parsers.Parser;
```

The [Ada Wiki Engine](https://github.com/stcarrez/ada-wiki) has a filter mechanism that
is used while parsing the input and before building the target wiki document instance.
Filters are chained together and a filter can do some work on the content it sees such as
blocking some content (filtering), collecting some data and doing some transformation on
the content.  When you want to use a filter, you have to declare an instance of the
corresponding filter type.

```
 with Wiki.Filters.Html;
 with Wiki.Filters.Autolink;
 with Wiki.Filters.TOC;
 ...
    Filter   : aliased Wiki.Filters.Html.Html_Filter_Type;
    Autolink : aliased Wiki.Filters.Autolink.Autolink_Filter;
    TOC      : aliased Wiki.Filters.TOC.TOC_Filter;
```

We use the `Autolink` filter that detects links in the text and transforms them into real links.
The `TOC` filter is used to collect header sections in the Wiki text and builds a table of content.
The `Html` filter is used to filter HTML content that could be contained in a Wiki text.
By default it ignores several HTML tags such as html, head, body, title, meta (these tags are silently discarded).
Furthermore it has the ability to hide several elements such as style and script (the tag and its content is discarded).

You will then configure the Wiki engine to build the filter chain and then define the Wiki syntax that the parser must use:

```
 Engine.Add_Filter (TOC'Unchecked_Access);
 Engine.Add_Filter (Autolink'Unchecked_Access);
 Engine.Add_Filter (Filter'Unchecked_Access);
 Engine.Set_Syntax (Syntax);
```

The Wiki engine gets its input from an `Input_Stream` interface that only defines a `Read`
procedure.  The [Ada Wiki Engine](https://github.com/stcarrez/ada-wiki) provides several
implementations of that interface, one of them is based on the Ada `Text_IO` package.
This is what we are going to use:

```
 with Wiki.Streams.Text_IO;
 ...
    Input    : aliased Wiki.Streams.Text_IO.File_Input_Stream;
```

You will then open the input file.  If the file contains UTF-8 characters, you may open it as follows:

```
 Input.Open (File_Path, "WCEM=8");
```

where `File_Path` is a string that represents the file's path.

Once the Wiki engine is setup and the input file opened, you can parse the Wiki text and build the Wiki document:

```
 Engine.Parse (Input'Unchecked_Access, Doc);
```

### Rendering a Wiki Document

After parsing a Wiki text you get a `Wiki.Documents.Document` instance that you can use as many times
as you want.  To render the Wiki document, you will first choose a renderer according to the target format
that you need.  The [Ada Wiki Engine](https://github.com/stcarrez/ada-wiki) provides three renderers:

* A Text renderer that produces text outputs,
* A HTML renderer that generates an HTML presentation for the document,
* A Wiki renderer that generates various Wiki syntaxes.

The renderer needs an output stream instance.  We are using the `Text_IO` implementation:

```
 with Wiki.Stream.Html.Text_IO;
 with Wiki.Render.Html;
 ...
    Output   : aliased Wiki.Streams.Html.Text_IO.Html_File_Output_Stream;
    Renderer : aliased Wiki.Render.Html.Html_Renderer;
```

You will then configure the renderer to tell it the output stream to use. You may enable or not
the rendering of Table Of Content and you just use the `Render` procedure to render the document.

```
 Renderer.Set_Output_Stream (Output'Unchecked_Access);
 Renderer.Set_Render_TOC (True);
 Renderer.Render (Doc);
```

By default the output stream is configured to write on the standard output.  This means that when
`Render` is called, the output will be written to the standard output.  You can choose another
output stream or open the output stream to a file according to your needs.



