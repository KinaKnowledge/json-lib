# JSON-Lib
### _Alex Nygren_

This is a simple json decoder that maps from Javascript to Common Lisp:

| Javascript      | Common Lisp   |
|-----------------|---------------|
|null             | NIL           |
|false            | NIL           |
|true             | T             |
|integer          | INTEGER       |
|float            | DOUBLE-FLOAT  |
|string           | String        |
|array            | VECTOR        |
|object           | HASH-TABLE    |


To convert encoded javascript, pass a string buffer to the json-lib:parse function.

## License

MIT



