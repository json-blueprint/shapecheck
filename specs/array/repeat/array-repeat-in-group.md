# zero or more repetitions inside of a group  
```jsbp
["start", Int*, "end"]
```

## empty array
```json
[]
```
+ Invalid
    - `.`

## array with mandatory content
```json
["start", "end"]
```
+ Valid

## array with single repetition
```json
["start", 1, "end"]
```
+ Valid

## array with two repetitions
```json
["start", 1, 2, "end"]
```
+ Valid

## array with three repetitions
```json
["start", 1, 2, 3, "end"]
```
+ Valid