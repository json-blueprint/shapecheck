# array group with optional items  
```jsbp
[Int?, String?, Boolean]
```

## array with optional items present
```json
[42, "foo", true]
```
+ Valid

## array with first optional item missing
```json
["foo", true]
```
+ Valid

## array with last optional item missing
```json
[42, true]
```
+ Valid

## array with both optional items missing
```json
[true]
```
+ Valid

## empty array (missing mandatory item)
```json
[]
```
+ Invalid
    - `.`