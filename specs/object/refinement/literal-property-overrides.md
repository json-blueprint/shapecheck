# literal property overrides
```jsbp-schema
Root = $Grandparent with $Parent with {
  foo: "child"
}

Parent = {
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
