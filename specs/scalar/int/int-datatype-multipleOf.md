# pattern for a multiple of some integer
```jsbp
Int(multipleOf=5)
```

## base number
```json
5
```
+ Valid

## positive multiple
```json
15
```
+ Valid

## negative multiple
```json
-15
```
+ Valid

## zero
```json
0
```
+ Valid

## not a multiple
```json
4
```
+ Invalid
    - `.`

## multiple in a decimal form
```json
15.0
```
+ Invalid
    - `.`