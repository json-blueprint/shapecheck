# array group with ambiguous items
A group pattern where nullable item have some overlap. 
```jsbp
[42?, Int?, 44, Boolean]
```

## array with all items present
The three numeric patterns are matched in sequence with items in the following array.
```json
[42, 43, 44, true]
```
+ Valid

## array with first optional item missing
```json
[20, 44, true]
```
`20` should be matched with `Int?`
+ Valid

## array with second optional item missing 
```json
[42, 44, true]
```
`44` is a valid value for `Int?`, but it can't be matched that way as the subsequent pattern (`44`) is mandatory, 
so `Int?` must be considered missing in this case
+ Valid

## array with all optional items missing 
```json
[44, true]
```
Again, `44` must be matched with the `44` literal pattern and not the preceding `Int?`.
+ Valid

## invalid array (missing trailing `Boolean`) 
```json
[44]
```
+ Invalid
    - `.`

## invalid array (first item is not an `Int`) 
```json
[4.5]
```
+ Invalid
    - `.[0]`