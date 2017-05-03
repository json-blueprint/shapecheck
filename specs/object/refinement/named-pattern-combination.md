# combination of named patterns
```jsbp-schema
Root = $Foo with $Bar with $Baz

Foo = {
  foo: "foo"
}

Bar = {
  bar: "bar"
}

Baz = {
  baz: "baz"
}
```

## doc with all required properties
```json
{
  "foo": "foo",
  "bar": "bar",
  "baz": "baz"
}
```
+ Valid

## doc missing the `foo` property
```json
{
  "bar": "bar",
  "baz": "baz"
}
```
+ Invalid
    - `.`


## doc missing the `bar` property
```json
{
  "foo": "foo",
  "baz": "baz"
}
```
+ Invalid
    - `.`

## doc missing the `baz` property
```json
{
  "foo": "foo",
  "bar": "bar"
}
```
+ Invalid
    - `.`
