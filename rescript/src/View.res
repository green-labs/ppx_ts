module Error = {
  type t
}

@ppx_ts.keyOf @ppx_ts.setType(Error.t) @ppx_ts.toGeneric @ppx_ts.partial
type t = {
  name: string,
  age: int,
}

let value = t_keyToString(Name)