module type Err = {
  type err = {
    firstName: string,
    lastName: string,
  }
}

module Err: Err = {
  type err = {
    firstName: string,
    lastName: string,
  }
}

@ppx_ts.keyOf
type t = {
  name: string,
  age: int,
}

type t1 = %ppx_ts.keyOf(Err.err)
type t2 = %ppx_ts.keyOf(t)
type t3 = %ppx_ts.toGeneric(Err.err)
type t4 = %ppx_ts.toGeneric(t)
type t5 = %ppx_ts.setType((t, Err.err))
