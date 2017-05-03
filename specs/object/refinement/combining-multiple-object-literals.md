# combining multiple object literals
```jsbp-schema
Root = {
  one: 1
} with $Foo with $Bar with {
  two: 2
}

Foo = {
  foo: "foo"
}

Bar = {
  bar: "bar"
}
```

## valid document
```json
{
  "one": 1,
  "foo": "foo",
  "bar": "bar",
  "two": 2
}
```
+ Valid
