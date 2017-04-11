# zero or more repetitions in an array  
```jsbp
[Int*]
```

## empty array
```json
[]
```
+ Valid


## singleton array
```json
[0]
```
+ Valid

## 10 element array
```json
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
```
+ Valid

## array with some invalid elements
```json
[0, 1, "not a number", 3, 4, false, 6, 7.7, 8, 9]
```
+ Invalid
    - `.[2]`
    - `.[5]`
    - `.[7]`