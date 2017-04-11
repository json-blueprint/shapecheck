# string datatype pattern with inclusive bounds
```jsbp
String(minLength=3, maxLength=5)
```

## string with matching length
```json
"abcd"
```
+ Valid

## string with lower-bound length
```json
"abc"
```
+ Valid

## string with upper-bound length
```json
"abcde"
```
+ Valid

## too short string
```json
"ab"
```
+ Invalid
    - `.`

## too long string
```json
"abcdef"
```
+ Invalid
    - `.`

## invalid type (boolean)
```json
true
```
+ Invalid
    - `.`