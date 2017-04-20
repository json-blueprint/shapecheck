# exactly X repetitions in an array
```jsbp
[Int{2}]
```

## empty array
```json
[]
```
+ Invalid
    - `.`

## singleton array
```json
[0]
```
+ Invalid
    - `.`

## 2 element array
```json
[0, 1]
```
+ Valid

## 3 element array
```json
[0, 1, 2]
```
+ Invalid
    - `.[2]`
