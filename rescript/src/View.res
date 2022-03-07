module type Err = {
  @spice
  type err = {
    firstName: string,
    lastName: string,
  }
}

module Err: Err = {
  @spice
  type err = {
    firstName: string,
    lastName: string,
  }
}

@ppx_ts.keyOf
type t = {
  name: string,
  age: int,
  isKorean: bool,
}

@spice
type t1 = %ppx_ts.keyOf(Err.err)
@spice
type t2 = %ppx_ts.keyOf(t)
@spice
type t3 = %ppx_ts.toGeneric(Err.err)
@spice
type t4 = %ppx_ts.toGeneric(t)
@spice
type t5 = %ppx_ts.setType((t, Err.err))
@spice
type t6 = %ppx_ts.setTypeExceptBool((t, string))
@spice
type t7 = %ppx_ts.partial(t)