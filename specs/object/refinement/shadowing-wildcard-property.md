# shadowing wildcard property
```jsbp-schema
Root = $Parent with {
  (version: Int)?,
  (/int.*/: Int)*
}

Parent = {
  id: Int,
  (String: String)*
}
```

## empty object
```json
{}
```
+ Invalid
    - `.`

## just the mandatory property
```json
{
  "id": 1
}
```
+ Valid

## with integer version
```json
{
  "id": 1,
  "version": 2
}
```
+ Valid

## with string version and some additional properties
```json
{
  "id": 1,
  "version": "a string should not be accepted",
  "anotherProp": "but here string is OK as specified by the Parent",
  "yetAnotherProp": "and here as well"
}
```
+ Invalid
    - `.version`

## with invalid `int.*` property value
Parent allows any property with String value, but this definition is shadowed in Root for any property starting with "int"
```json
{
  "id": 1,
  "intProperty": "a string should not be accepted"
}
```
+ Invalid
    - `.intProperty`

## valid doc with both int and string properties
```json
{
  "id": 1,
  "version": 2,
  "foo": "bar",
  "intProp": 3,
  "stringProp": "baz",
  "intIsNice": 4
}
```
+ Valid
