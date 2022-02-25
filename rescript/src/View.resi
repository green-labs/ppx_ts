module type Error = {
  type t = {
    firstName: string,
    lastName: string,
  }
}

@ppx_ts.keyOf @ppx_ts.toGeneric
type t = {
  name: string,
  age: int,
}

type t1 = %ppx_ts.keyOf(Error.t)
type t2 = %ppx_ts.keyOf(t)
type t3 = %ppx_ts.toGeneric(Error.t)
type t4 = %ppx_ts.toGeneric(t)