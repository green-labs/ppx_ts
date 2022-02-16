# PPX_ts

## Features

### `keyOf`

```rescript
@ppx_ts.keyOf
type t = {
  name: string,
  age: int
}

// automatically generated
type t_keyof = Name | Age

// automatically generated
let t_keyToString = key =>
  switch key {
  | Name => "name"
  | Age => "age"
  }
```

### `setType(t)`

```rescript
module Error = {
  type t
}
@ppx_ts.setType(Error.t)
type t = {
  name: string,
  age: int
}

// automatically generated
type t_setType = {
  name: Error.t
  age: Error.t
}
```

### `toGeneric`

```rescript
@ppx_ts.toGeneric
type t = {
  name: string,
  age: int
}

// automatically generated
type t_toGeneric<'a> = {
  name: 'a
  age: 'a
}
```

### `partial`

```rescript
@ppx_ts.partial
type t = {
  name: string,
  age: int
}

// automatically generated
type t_partial = {
  name: option<string>,
  age: option<int>
}
```
