open Ppxlib

(* open Parsetree *)
open Ast_helper

let attribute_name = "ppx_ts"

type attribute_kind =
  | KeyOf of string * payload
  | SetType of string * payload
  | ToGeneric of string * payload
  | Partial of string * payload

let suffix_key_of = "keyOf"
let suffix_set_type = "setType"
let suffix_to_generic = "toGeneric"
let suffix_partial = "partial"
let suffix_key_to_string = "keyToString"

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

let parse_attribute { attr_name = { Location.txt }; attr_payload } =
  if txt = mk_attr_with_suffix attribute_name suffix_key_of then
    Some (KeyOf (suffix_key_of, attr_payload))
  else if txt = mk_attr_with_suffix attribute_name suffix_set_type then
    Some (SetType (suffix_set_type, attr_payload))
  else if txt = mk_attr_with_suffix attribute_name suffix_to_generic then
    Some (ToGeneric (suffix_to_generic, attr_payload))
  else if txt = mk_attr_with_suffix attribute_name suffix_partial then
    Some (Partial (suffix_partial, attr_payload))
  else None
