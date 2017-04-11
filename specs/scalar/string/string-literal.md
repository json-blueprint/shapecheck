# string literal pattern
```jsbp
"foo"
```

## matching literal
```json
"foo"
```
+ Valid

## different string literal
```json
"bar"
```
+ Invalid
    - `.`

## correct string literal with leading whitespace
```json
" foo"
```
+ Invalid
    - `.`

## correct string literal with trailing whitespace
```json
"foo "
```
+ Invalid
    - `.`

## invalid type (boolean)
```json
true
```
+ Invalid
    - `.`