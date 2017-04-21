# object with a single property
```jsbp
{
  "foo": String
}
```

## empty object
```json
{}
```
+ Invalid
    - `.`


## matching object
```json
{
  "foo": "bar"
}
```
+ Valid

## object with bad value type
```json
{
  "foo": 1
}
```
+ Invalid
    - `.foo`

## object with additional unexpected property
```json
{
  "foo": "bar",
  "bar": false
}
```
+ Invalid
    - `.bar`

## object with both bad value and an unexpected property
```json
{
  "foo": 1,
  "bar": false
}
```
+ Invalid
    - `.foo`
    - `.bar`
