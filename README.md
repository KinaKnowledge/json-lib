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

**parse** *[json-lib] input-string &key use-keywords-for-keys trace (max-depth 1000) (max-exponent-length 2))*

Given an encoded UTF-8 JSON string, `parse` returns a Common Lisp structure or value.  If use-keywords-for-keys is T, then hash table keys will be constructed as keywords. By default the limit is 1000 for structural depth, but this can be set with the keyword max-depth. Exponent representation in serialized form is limited to a length of 2 to prevent huge values causing slow downs and other issues in the conversion process.


**stringify** *[json-lib] data &key (case-encoder #'lisp-to-snakecase) unencodable-items)*

`Stringify` converts the given data structure to a stringified JSON form, suitable for serialization and other uses. An optional function can be passed for case encoding of native lisp keyword structures. If a function for unencodable-items is provided, this function will be called, and should return a JSON compliant string representing the encoded items. If no value is provided for unencodable-items, the JSON null value is used.


## Examples

### Parsing JSON to Common Lisp

To convert encoded JSON, pass a UTF-8 string buffer to the json-lib:parse function.  For example, to read and parse a JSON encoded file:

	(json-lib:parse (alexandria:read-file-into-string "file.json" 
                                                      :external-format :utf8))

**Note that it is important to specify in your input stream conversions that UTF-8 should be used.**



### Serializing Common Lisp Structures to JSON

To get a serialized JSON string, pass a Lisp structure constructed with the listed types for Common Lisp in the Mappings table to the `stringify` function.  

	(json-lib:stringify #(1 2 3.14159 #("ABC" T "♥" nil)))
	
This will output a string as follows:

	"[1, 2, 3.14159, [\"ABC\", true, \"♥\", null]]"


### Specifics and Notes

This library is fairly forgiving from a parsing standpoint.  Particularly, in parsing JSON, and in contrast with the JSON spec,
commas are not required to delimit input JSON.  The parser uses whitespace presence, regardless of commas, as a delimiting marker.

Compliance is measured to the [JSONTestSuite](https://github.com/nst/JSONTestSuite) Github repository tests.  The (tests/compliance.lisp) has a function `build-parser-test-harness` that will clone the JSONTestSuite repo into a subfolder in your current working directory and report out to STDOUT the validation results, plus returns a hashtable.  A copy of the output will be added to this repo as a reference guide.

## License

MIT



