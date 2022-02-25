module Error = {
  type t = {
    firstName: string,
    lastName: string,
  }
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

type t1 = %ppx_ts.keyOf(Error.t)
type t2 = %ppx_ts.keyOf(t)

let value = t_keyToString(Name)