# implicit array group
Array content is a group with items that match positionally.  
```jsbp
[Int, String, Boolean]
```

## array with matching items
```json
[42, "foo", true]
```
+ Valid

## array with missing last item
```json
[42, "foo"]
```
+ Invalid
    - `.`

## array with too many items
```json
[42, "foo", true, "this item should not be here"]
```
+ Invalid
    - `.[3]`
    
## array with invalid first item
```json
[null, "foo", true]
```
+ Invalid
    - `.[0]`
    
## array with all items invalid
```json
[null, 111, "should have been boolean"]
```
+ Invalid
    - `.[0]`
    - `.[1]`
    - `.[2]`