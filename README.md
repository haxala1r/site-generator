# What *is* this?

This project (haven't decided on a name yet...) is a static website generator.
It generates HTML code from the combination of some Common Lisp markup
and a usable but incomplete (as of yet) subset of Markdown.

## How to build
Load the build.lisp program using your favourite Common Lisp implementation
(note that I have only tested using SBCL and ECL). This should generate an
executable named `site-generator` in the current directory.

## How to use
Invoke the generated executable through a shell, like so:

`./site-generator <template-file> {one or more markdown files...}`

This will generate an html file corresponding to each markdown file supplied.
The generated HTML depends on the template and the markdown file.