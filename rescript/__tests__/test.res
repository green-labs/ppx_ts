open Jest
open Expect

describe("keyOf", _ => {
  test("t", _ => {
    let nameT: View.t_keyOf = View.Name
    let nameTInString = nameT->View.t_keyToString
    expect(nameTInString) |> toEqual("name")
  })
  test("t1", _ => {
    let nameT1: View.t1 = View.FirstName
    let nameT1InString = nameT1->View.t1_keyToString
    expect(nameT1InString) |> toEqual("firstName")
  })
  test("t2", _ => {
    let nameT2: View.t2 = View.Name
    let nameT2InString = nameT2->View.t2_keyToString
    expect(nameT2InString) |> toEqual("name")
  })
})