# integer datatype pattern with inclusive bounds
```jsbp
Int(min=3, max=5)
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
+ Valid

## upper bound integer
```json
5
```
+ Valid

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
> StrictInt
```json
4.0
```
+ Invalid
    - `.`
