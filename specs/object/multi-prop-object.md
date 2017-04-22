# object with multiple properties
```jsbp
{
  foo: String,
  bar: Int,
  baz: Boolean
}
```

## empty object
```json
{}
```
+ Invalid
    - `.`

## matching object (same property order)
```json
{
  "foo": "foo",
  "bar": 42,
  "baz": true
}
```
+ Valid

## matching object (reverse property order)
```json
{
  "baz": true,
  "bar": 42,
  "foo": "foo"
}
```
+ Valid

## object with single missing property
```json
{
  "bar": 42,
  "baz": true
}
```
+ Invalid
    - `.`

## object with additional property
```json
{
  "foo": "foo",
  "bar": 42,
  "baz": true,
  "notSupposedToBeHere": true
}
```
+ Invalid
    - `.notSupposedToBeHere`

## object with invalid property values
```json
{
  "foo": 0,
  "bar": 42,
  "baz": "should have been boolean"
}
```
+ Invalid
    - `.foo`
    - `.baz`
