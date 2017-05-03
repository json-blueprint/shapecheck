# simple object refinement
```jsbp-schema 
Root = $Page with {
  items: [$User*]
}

User = {
  login: String
}

Page = {
  links: {
    self: $Uri,
    (prev: $Uri)?,
    (next: $Uri)?
  }
}

Uri = String
```

## empty page
```json
{
  "items": [],
  "links": {
    "self": "/users"
  }
}
```
+ Valid

## non-empty first page 
```json
{
  "items": [
    { "login": "foobar" },
    { "login": "barbar" }
  ],
  "links": {
    "self": "/users",
    "next": "/users?page=1"
  }
}
```
+ Valid

## non-empty second page
```json
{
  "items": [
    { "login": "foobar" },
    { "login": "barbar" }
  ],
  "links": {
    "self": "/users?page=1",
    "prev": "/users",
    "next": "/users?page=2"
  }
}
```
+ Valid

## page with missing self-link
```json
{
  "items": []
}
```
+ Invalid
    - `.`

## page with missing items
```json
{
  "links": {
    "self": "/users"
  }
}
```
+ Invalid
    - `.`
