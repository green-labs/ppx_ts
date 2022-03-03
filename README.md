# PPX_ts

`ppx_ts` is a ReScript preprocessor, which helps generating helper types to bind to typescript modules.

## Install

```rescript
yarn add -D @greenlabs/ppx_ts
```

```rescript
// bsconfig.json
"ppx-flags": [
  ...,
  "@greenlabs/ppx_ts/ppx"
],
```

## Features

### Attributes `@`

#### `keyOf`

```rescript
@ppx_ts.keyOf
type t = {
  name: string,
  age: int
}

// automatically generated
type t_keyOf = Name | Age

// automatically generated
let t_keyToString = key =>
  switch key {
  | Name => "name"
  | Age => "age"
  }
```

#### `setType(t)`

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

#### `toGeneric`

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

#### `partial`

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

#### `pick`

```rescript
@ppx_ts.pick(["name"])
type t = {
  name: string,
  age: int
}

// automatically generated
type t_pick_name = {
  name: string,
}
```

#### `omit`

```rescript
@ppx_ts.omit(["name"])
type t = {
  name: string,
  age: int
}

// automatically generated
type t_omit_name = {
  age: int,
}
```

### Extension `%`

#### `keyOf`

```rescript
module Error = {
  type t = {
    name: string,
    age: int
  }
}

type t1 = %ppx_ts.keyOf(Error.t) // type t1 = Name | Age

// automatically generated
let t1_keyToString = key =>
  switch key {
  | Name => "name"
  | Age => "age"
  }
```

#### `toGeneric`

```rescript
type t = {
  name: string,
  age: int
}

type t1 = %ppx_ts.toGeneric(t)
// changed
type t1<'a> = {
  name: 'a
  age: 'a
}
```

#### `setType`

```rescript
type t = {
  name: string,
  age: int
}

@spice // attributes are available
type t1 = %ppx_ts.setType((t, string))
// changed
type t1 = {
  name: string
  age: string
}
```

## Contribution

1. Create a sandbox with opam

```
opam switch create ppx_ts 4.12.1
```

2. Install dependencies

```
opam install . --deps-only --with-test
```

3. Build

```
opam exec -- dune build
```

4. Test

```
cd rescript

(install dependencies)
yarn

(build --watch)
yarn res:clean && yarn res:watch

(run test --watch)
yarn test:watch
```
