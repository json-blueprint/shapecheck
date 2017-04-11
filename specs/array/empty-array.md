# empty array pattern
```jsbp
[]
```

## empty array
```json
[]
```
+ Valid

## non-empty array
```json
[1]
```
+ Invalid
    - `.[0]`

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