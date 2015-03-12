For sum types, Deca emits an LLVM structure type corresponding to the largest (size in bytes) alternative of the variant, tagged with a variant case-tag. This ensures enough memory is allocated to contain any possible value of the variant type. On the other hand, no runtime type information is included, only the variant case-tag.  Deca sum types thus compile down, in the general case, to an LLVM structure with no data not explicitly specified by the programmer.

Pattern matching consists of matching against variant case-tags, matching against constant data values (including possibly enumeration values), destructuring records, and simply binding the given data to a new variable. These different kinds of matches are composed into a pattern, allowing pattern-matching similar to that available in Scala[NEEDED](CITATION.md).

## Deca's pattern-matching syntax ##

_pattern\_list_ = {one} _match\_pattern_ |
> {many} _pattern\_list_ , _match\_pattern_

_pattern\_variable_ = {some} _unqualified\_identifier_ | {none} `_`

_match\_pattern_ = {literal} _literal\_expression_ |
> {variable} `[`name`]`:_pattern\_variable_ _type\_annotation_? |
> {variant} `[`name`]`:_qualified\_identifier_ ( _pattern\_list_? ) |
> {tuple} `[` _pattern\_list_ `]`

_match\_case\_clause_ = **case** `[`pattern`]`:_match\_pattern_ => `[`body`]`:_expression_

_match\_expression_ = **match** ( _expression_ ) { _match\_case\_clause_+ } _else\_clause_?