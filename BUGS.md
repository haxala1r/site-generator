Currently, (because the unordered-list-parser function always consumes
the next newline) you have to put two extra newlines after an unordered list
in a paragraph. Otherwise if a header comes immediately after that paragraph,
it will not be parsed properly. I don't really know how to fix this... yet!

Also, I should maybe change most of the parsers so that space is actually a token
as well. This would make formatting a little more intuitive, and it would allow 
a bit saner behaviour as well.
