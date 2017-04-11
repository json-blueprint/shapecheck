# numeric literal pattern
```jsbp
4.2
```

## matching literal
```json
4.2
```
+ Valid

## different numeric literal
```json
2.4
```
+ Invalid
    - `.`

## matching number with trailing zeros in decimal part 
```json
4.20
```
+ Valid

## invalid type (boolean)
```json
true
```
+ Invalid
    - `.`