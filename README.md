# PPX_ts

## Features

### `keyof`

```rescript
@ppx_ts
type t = {
  name: string,
  age: int
}

// automatically generated
type t_keyof = NAME | AGE
```

