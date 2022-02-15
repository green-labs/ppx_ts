open Jest
open Expect

describe("", _ => {
  test("", _ => {
    let name = View.Name
    let stringifyName = switch name {
    | Name => "name"
    | Age => "age"
    }
    expect(stringifyName) |> toEqual("name")
  })
})
