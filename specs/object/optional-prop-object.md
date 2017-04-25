# object with optional property
```jsbp
{
  foo: Int,
  (bar: String)?,
  baz: Boolean
}
```

## optional property missing
```json
{
  "foo": 1,
  "baz": true
}
```
+ Valid

## optional property present
```json
{
  "foo": 1,
  "bar": "yep, it's here",
  "baz": true
}
```
+ Valid

## optional property with invalid value
```json
{
  "foo": 1,
  "bar": 1,
  "baz": true
}
```
+ Invalid
    - `.bar`
