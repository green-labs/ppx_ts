open Ppxlib
open Parsetree
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

(* make constructor declaration with label *)
let make_const_decls labels loc =
  labels
  |> List.map (fun label -> String.capitalize_ascii label)
  |> List.map (fun label -> Type.constructor ~loc (mkloc label loc))

let make_match_case labels =
  labels
  |> List.map (fun key ->
         Exp.case
           (Pat.construct (lid (String.capitalize_ascii key)) None)
           (Exp.constant (Const.string key)))

(* make label_declaration with new type constructor using lid *)
let make_label_decls_with_core_type ?(is_option = false) decls core_type =
  decls
  |> List.map (fun { pld_name; pld_loc } ->
         Type.field ~loc:pld_loc pld_name
           (match is_option with
           | true -> Typ.constr (lid "option") [ core_type ]
           | false -> core_type))

(* make label_declaration with is_option flag *)
let make_label_decls ?(is_option = false) decls =
  decls
  |> List.map (fun { pld_name; pld_type; pld_loc } ->
         Type.field ~loc:pld_loc pld_name
           (match is_option with
           | true -> Typ.constr (lid "option") [ pld_type ]
           | false -> pld_type))
