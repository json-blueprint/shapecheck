# empty object pattern
```jsbp
{}
```

## empty object
```json
{}
```
+ Valid

## non-empty object
```json
{
  "foo": "bar"
}
```
+ Invalid
     - `.foo`

## invalid type (string)
```json
"foo"
```
+ Invalid
    - `.`

## invalid type (number)
```json
1
```
+ Invalid
    - `.`

## invalid type (boolean)
```json
true
```
+ Invalid
    - `.`

## invalid type (null)
```json
null
```
+ Invalid
    - `.`

## invalid type (array)
```json
[]
```
+ Invalid
    - `.`
