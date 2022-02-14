open Jest
open Expect

describe("", _ => {
  test("", _ => {
    let name = View.NAME
    let stringifyName = switch name {
    | NAME => "name"
    | AGE => "age"
    }
    expect(stringifyName) |> toEqual("name")
  })
})
