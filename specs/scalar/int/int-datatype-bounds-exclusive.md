# integer datatype pattern with exclusive bounds
```jsbp
Int(minExclusive=3, maxExclusive=5)
```

## integer within bounds
```json
4
```
+ Valid

## lower bound integer
```json
3
```
+ Invalid
    - `.`

## upper bound integer
```json
5
```
+ Invalid
    - `.`

## integer smaller than lower bound
```json
2
```
+ Invalid
    - `.`

## integer higher than upper bound
```json
6
```
+ Invalid
    - `.`

## decimal within bounds
```json
4.2
```
+ Invalid
    - `.`

## decimal with int value within bounds
```json
4.0
```
+ Invalid
    - `.`