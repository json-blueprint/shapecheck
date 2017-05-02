# self-recursive pattern
```jsbp-schema
Root = [Int, $Root?]
```

## no recursion
```json
[1]
```
+ Valid

## invalid with no recursion
```json
["not a number"]
```
+ Invalid
    - `.[0]`

## single recursion
```json
[1, [2]]
```
+ Valid

## invalid single recursion
```json
[1, ["not a number"]]
```
+ Invalid
    - `.[1].[0]`

## double recursion
```json
[1, [2, [3]]]
```
+ Valid

## invalid double recursion
```json
[1, [2, ["not a number"]]]
```
+ Invalid
    - `.[1].[1].[0]`

## tripple recursion
```json
[1, [2, [3, [4]]]]
```
+ Valid

## invalid tripple recursion
```json
[1, [2, [3, ["not a number"]]]]
```
+ Invalid
    - `.[1].[1].[1].[0]`
