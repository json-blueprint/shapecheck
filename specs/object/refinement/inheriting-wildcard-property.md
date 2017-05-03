# inheriting wildcard property
```jsbp-schema
Root = $Extensible with {
  item: $Item
}

Item = $Extensible with {
  name: String
}

Extensible = {
  (String: Any)*
}
```

## document with just the mandatory stuff
```json
{
  "item": {
    "name": "FooBar"
  }
}
```
+ Valid

## document with additional properties
```json
{
  "item": {
    "name": "FooBar",
    "string": "foo",
    "int": 1,
    "boolean": true,
    "null": null
  },
  "string": "foo",
  "int": 1,
  "boolean": true,
  "null": null
}
```
+ Valid

## document with additional properties and invalid `item.name` value
```json
{
  "item": {
    "name": 1,
    "string": "foo",
    "int": 1,
    "boolean": true,
    "null": null
  },
  "string": "foo",
  "int": 1,
  "boolean": true,
  "null": null
}
```
+ Invalid
    - `.item.name`

## document with additional properties and invalid `item` value
```json
{
  "item": "an object expected here",
  "string": "foo",
  "int": 1,
  "boolean": true,
  "null": null
}
```
+ Invalid
    - `.item`
