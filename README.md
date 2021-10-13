# JSON-Lib
### JSON Parser and Serializer For Common Lisp

This is a simple json decoder and encoder that maps from JSON to Common Lisp and vice versa.  The mappings are described in the following table.  It strives to ensure no loss of fidelity in conversions of structured data between the two forms of representation.  

### Mappings 

| JSON            | Common Lisp   |
|-----------------|---------------|
|null             | NIL           |
|false            | NIL           |
|true             | T             |
|integer          | INTEGER       |
|float            | DOUBLE-FLOAT  |
|string           | String        |
|array            | VECTOR        |
|object           | HASH-TABLE    |



## How to Use

This library has two primary functions: `parse`, which takes a UTF-8 encoded JSON string and builds a representation of the structure in Common Lisp, and `stringify`, which takes a Common Lisp input structure and returns an output JSON string.  


|FUNCTION      | INPUT                 | OUTPUT                     |
|--------------|-----------------------|----------------------------|
|PARSE         | UTF-8 Encoded Strings | Common Lisp (see Mappings) |
|STRINGIFY     | Common Lisp           | Common Lisp String         |


### Parsing JSON to Common Lisp

To convert encoded JSON, pass a UTF-8 string buffer to the json-lib:parse function.  For example, to read and parse a JSON encoded file:

	(json-lib:parse (alexandria:read-file-into-string "file.json" 
                                                      :external-format :utf8))

**Note that it is important to specify in your input stream conversions that UTF-8 should be used.**



## License

MIT



