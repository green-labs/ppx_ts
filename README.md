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
type t_keyof = NAME | AGE
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
