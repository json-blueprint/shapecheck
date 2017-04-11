# numeric datatype pattern with exclusive bounds
```jsbp
Number(minExclusive=3.1, maxExclusive=5.2)
```

## whole number within bounds
```json
4
```
+ Valid

## decimal number withing bounds
```json
4.2
```
+ Valid

## number just above lower bound
```json
3.101
```
+ Valid

## number just above below upper bound
```json
5.199
```
+ Valid

## lower bound number 
```json
3.1
```
+ Invalid
    - `.`

## upper bound number 
```json
5.2
```
+ Invalid
    - `.`

## number smaller than lower bound
```json
3.099
```
+ Invalid
    - `.`

## number higher than upper bound
```json
5.201
```
+ Invalid
    - `.`