# array with optional group
```jsbp
[(1, 2, 3)?]
```

## empty array
```json
[]
```
+ Valid

## array with incomplete group (just 1st item)
```json
[1]
```
+ Invalid
    - `.`

## array with incomplete group (first 2 items)
```json
[1, 2]
```
+ Invalid
    - `.`

## array with complete group
```json
[1, 2, 3]
```
+ Valid
