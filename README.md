# PINSCompiler
This is my PINS compiler.

## Phases
### LEX (12h)
- keywords
  - `[arr, else, for, fun, if, then, typ, var, where, while]`
- atom types
  - `[logical integer, string]`
- constants of atom types
  - `logical`:`[true, false]`
  - `integer`:`[0, 1, 2, 3, 4, 5, 6456, 2345897234, ....]`
  - `string`:`['', 'a', 'abc', 'a b c', 'don''t' ....]`
    - only one line
- names
  - letters
  - numbers
  - underscores
  - can't start with a number
- operators
  - `+ - * / %`
  - `& |`
  - `! == < > <= >= !=`
  - `() [] {}`
  - `: ; . , =`
- comments
  - from `#` to the end of the line
- white space
  - space
  - tab (4 spaces)
  - newline
  - carriage return

Returns a list of tokens with their tokens, lexemes,
and positions in the source file.

### SYN (6-7h)
Rewrote grammar in `./pins.pdf` to grammar in `./gramatika.txt`.
Wrote parser using recursive descent. Method for every
non-terminal in the grammar.

### AST (5h)
Methods from SYN return AST nodes. AST nodes are
defined in `./src/compiler/parser/ast/`.

With this we can generate an abstract syntax tree.

### NAME CHECKING (4h)
Checks all the names in the AST and connects each name to its
declaration. Also checks for name collisions and all that fun
stuff.

First time using a visitor pattern. Didn't use all the prepared
methods. Can find code in `./src/compiler/seman/name/`.

### TYPE CHECKING (6-7h)
Checks every type it can find and figures out if types are all ok.
Here we also check for type cycles using DFS. 

Types must match based on what kind of expression we are using.
