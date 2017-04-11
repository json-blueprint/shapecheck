# numeric literal pattern with scientific notation
```jsbp
42e-1
```

## matching literal
```json
4.2
```
+ Valid

## matching literal in scientific notation
```json
42e-1
```
+ Valid

## matching literal in scientific notation (different exponent)
```json
420e-2
```
+ Valid

## different int literal in scientific notation
```json
42e-3
```
+ Invalid
    - `.`