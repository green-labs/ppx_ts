open Ppxlib

(* open Parsetree *)
open Ast_helper

let attribute_name = "ppx_ts"

type attribute_kind =
  | KeyOf of string
  | SetType of string
  | ToGeneric of string

let suffix_key_of = "keyOf"
let suffix_set_type = "setType"
let suffix_to_generic = "toGeneric"

(* make attribute name to suffix string *)
let mk_attr_with_suffix attr_name suffix = attr_name ^ "." ^ suffix
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

let parse_attribute { attr_name = { Location.txt } } =
  if txt = mk_attr_with_suffix attribute_name suffix_key_of then
    Some (KeyOf suffix_key_of)
  else if txt = mk_attr_with_suffix attribute_name suffix_set_type then
    Some (SetType suffix_set_type)
  else if txt = mk_attr_with_suffix attribute_name suffix_to_generic then
    Some (ToGeneric suffix_to_generic)
  else None
