module Error = {
  type t
}

@ppx_ts.keyOf
@ppx_ts.setType(Error.t)
@ppx_ts.toGeneric
@ppx_ts.partial
@ppx_ts.pick(["name"])
@ppx_ts.omit(["name"])
type t = {
  name: string,
  age: int,
}

type t1 = %ppx_ts.keyOf(t)

let value = t_keyToString(Name)