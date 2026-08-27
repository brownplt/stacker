# README.md

This directory defines a CodeMirror language mode for Rhombus. The definition is adapted from
[the example given by the CodeMirror document](https://codemirror.net/examples/lang-package/),
following the same shape as the other modes here.

It is a highlighting grammar rather than a shrubbery parser: it recognises tokens and bracket
nesting and leaves the layout rules alone. Rebuild `parser.js` with `npm run build-rhombus-parser`
after editing `rhombus.grammar`.
