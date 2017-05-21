# shapecheck: a JSON Blueprint validator for JavaScript
**shapecheck** validates structure of JSON based on a schema defined in [JSON Blueprint](httpjson-blueprint.org).

## Installation
`npm install --save shapecheck`

## Project Status
Both JSON Blueprint and shapecheck are pre 1.0. There might be some changes to both schema syntax and validation API. Only use in production if you're feeling adventurous.

Also, the library is implemented in PureScript and because of that is quite large – currently around 260K with no external dependencies. If you plan to use it on the client, be sure to enable gzip compression, which will bring it to something around 40K.

## Validation
```javascript
var schema = 'Person = { firstName: String, lastName: String }';
var validator = shapecheck.createValidator(schema);
validator.validate('Person', { firstName: "John", lastName: "Doe" });
```
First, you have to create a validator for the given schema. If you validate multiple JSON documents against the same schema, you should reuse the `validator` to prevent repeated parsing of the schema.

Each `validate` call takes 2 arguments:

* `patternName`: name of one of the patterns from schema
* `json`: parsed JSON to match against the pattern

Shape of the validation result is described by the following schema:
```
Result = {
    valid: Boolean,
    errors: [Error*]
}

Error = {
    message: String,

    /**
     * Path in the JSON document where the validation error occurred
     */
    path: [(Int | String)*],

    /**
     * String representation of the path (jq-like)
     */
    pathString: String,

    /**
     * String representation of the pattern for the JSON value at path
     */
    patternString: String,

    /**
     * When pattern is a choice, the children array contains validation
     * errors from all of its branches
     */
    children: [Error*]
}
```
