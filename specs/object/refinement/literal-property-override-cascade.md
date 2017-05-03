# literal property override cascade
```jsbp-schema
Root = $Parent with {
  foo: "child"
}

Parent = $Grandparent with {
  foo: "parent",
  bar: "parent"
}

Grandparent = {
  foo: "grandparent",
  bar: "grandparent",
  baz: "grandparent"
}
```

## ancestor properties with the same name should be overridden
```json
{
  "foo": "child",
  "bar": "parent",
  "baz": "grandparent"
}
```
+ Valid
