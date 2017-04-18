# Value choice pattern
```jsbp
Int(min = 0) | String(minLength = 5) | "foo"
```

## Int value matching the first pattern
```json
42
```
+ Valid

## String value matching the second pattern
```json
"markdown"
```
+ Valid

## String value matching the third pattern
```json
"foo"
```
+ Valid

## Value of unexpected type (Boolean)
```json
true
```
+ Invalid
    - `.`

## Int value that doesn't meet the `min` constraint
```json
-1
```
+ Invalid
    - `.`

## String value that doesn't meet the `minLength` requirement and doesn't match allowed String literal
```json
"bar"
```
+ Invalid
    - `.`
