# simple schema
```jsbp-schema
Root = $Foo | $Bar

Foo = "foo"

Bar = "bar"
```

## foo doc
```json
"foo"
```
+ Valid

## bar doc
```json
"bar"
```
+ Valid

## something else
```json
1
```
+ Invalid
    - `.`
