# object with additional property matching a regex
```jsbp
{
  foo: Int,
  bar: Int,
  /ext.*/: String
}
```

## fixed properties take precedence over regex ones
Both `foo` and `bar` allow only `Int`s, and there can be one more property with any name that starts with `ext`, but it has to have `String` value.
```json
{
  "extension": "a string",
  "foo": 1,
  "bar": 2
}
```
+ Valid

## property with name not matching the regex
```json
{
  "mumbojumbo": "a string",
  "foo": 1,
  "bar": 2
}
```
+ Invalid
    - `.mumbojumbo`
    - `.`

## property with name which would match if the regex wasn't anchored
A regex for property name should always match fully - in this case, it would match if we were only looking for matching substrings.
```json
{
  "unextended": "a string",
  "foo": 1,
  "bar": 2
}
```
+ Invalid
    - `.unextended`
    - `.`

## property with matching exotic name
```json
{
  "ext#%^!*#*.,?": "a string",
  "foo": 1,
  "bar": 2
}
```
+ Valid

## property with invalid value
```json
{
  "extension": false,
  "foo": 1,
  "bar": 2
}
```
+ Invalid
    - `.extension`

## multiple additional properties when pattern only allows one
```json
{
  "extension": "a string",
  "foo": 1,
  "bar": 2,
  "extension2": "a string"
}
```
+ Invalid
    - `.extension2`
