# TODO

- [x] generate element
- [ ] add comment element or as a token
- [x] correctly generate void elements, eg. \<link>
    - [x] modify the AST, and Parser
    - I use semicolon for void element
- [x] format and indent the output
    - is there any libray for this? (yes, I'm using prettyprinter)
- [ ] escape the input
    - do i need a library for this?
- [x] `translateFile` output file naming
    - [x] it was only append `.html` after the input file name
