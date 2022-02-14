open Ppxlib
(* open Parsetree *)
open Ast_helper

let attribute_name = "ppx_ts"
let attribute_name_keyof = "keyof"

let loc = !default_loc

let fail loc message = Location.raise_errorf ~loc "%s" message

let mkloc txt loc = { Location.txt; loc }

let mknoloc txt = mkloc txt Location.none

let lid ?(loc = Location.none) s = mkloc (Longident.parse s) loc

let get_attribute_by_name attributes name =
  let filtered =
    attributes
    |> List.filter (fun { attr_name = { Location.txt } } -> txt = name)
  in
  match filtered with
  | [] -> Ok None
  | [ attribute ] -> Ok (Some attribute)
  | _ -> Error ("Too many occurrences of \"" ^ name ^ "\" attribute")
