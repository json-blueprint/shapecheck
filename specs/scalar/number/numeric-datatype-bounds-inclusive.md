# numeric datatype pattern with inclusive bounds
```jsbp
Number(min=3.1, max=5.2)
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

## lower bound number 
```json
3.1
```
+ Valid

## upper bound number 
```json
5.2
```
+ Valid

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