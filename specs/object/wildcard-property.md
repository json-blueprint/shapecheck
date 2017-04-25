# object with wildcard (`String`) property name
```jsbp
{
  foo: Int,
  String: String,
  bar: Int
}
```

## fixed properties take precedence over wildcard ones
Both `foo` and `bar` allow only `Int`s, and there can be one more property with any name, but it has to have `String` value.
```json
{
  "wildcard": "a string",
  "foo": 1,
  "bar": 2
}
```
+ Valid

## wildcard property with exotic name
```json
{
  "#%^!*#*.,?": "a string",
  "foo": 1,
  "bar": 2
}
```
+ Valid

## wildcard property with invalid value
```json
{
  "wildcard": false,
  "foo": 1,
  "bar": 2
}
```
+ Invalid
    - `.wildcard`

## multiple additional properties when pattern only allows one
```json
{
  "wildcard": "a string",
  "foo": 1,
  "bar": 2,
  "wildcard2": "a string"
}
```
+ Invalid
    - `.wildcard2`
