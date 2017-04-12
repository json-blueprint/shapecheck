# integer literal pattern
```jsbp
420
```

## matching literal
```json
420
```
+ Valid

## matching literal in scientific notation
```json
42e1
```
+ Valid

## different int literal
```json
0
```
+ Invalid
    - `.`

## decimal with the same numeric value
```json
420.0
```
+ Valid

## invalid type (boolean)
```json
true
```
+ Invalid
    - `.`
