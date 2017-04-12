# integer literal pattern with scientific notation
```jsbp
42e2
```

## matching literal
```json
4200
```
+ Valid

## matching literal in scientific notation
```json
42e2
```
+ Valid

## matching literal in scientific notation (different exponent)
```json
4.2e3
```
+ Valid

## different int literal in scientific notation
```json
42e3
```
+ Invalid
    - `.`
