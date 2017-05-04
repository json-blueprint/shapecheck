# shapecheck: a JSON Blueprint validator for JavaScript
**shapecheck** validates structure of JSON based on a schema defined in [JSON Blueprint](https://json-blueprint.org).

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
    path: String,
    pattern: String,
    children: [Error*]
}
```