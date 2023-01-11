open Ppxlib
open Parsetree
open Ast_helper

let attribute_name = "ppx_ts"

type attribute_kind =
  | KeyOf of string * payload
  | SetType of string * payload
  | ToGeneric of string * payload
  | Partial of string * payload
  | Pick of string * payload
  | Omit of string * payload
  | ToArray of string * payload

type extension_kind =
  | KeyOf of string * string list * payload * attributes
  | SetType of string * string list * payload * attributes
  | SetTypeExceptBool of string * string list * payload * attributes
  | ToGeneric of string * string list * payload * attributes
  | Partial of string * string list * payload * attributes
  | Pick of string * string list * payload * attributes
  | Omit of string * string list * payload * attributes

let suffix_key_of = "keyOf"
let suffix_set_type = "setType"
let suffix_set_type_except_bool = "setTypeExceptBool"
let suffix_to_generic = "toGeneric"
let suffix_partial = "partial"
let suffix_pick = "pick"
let suffix_omit = "omit"
let suffix_key_to_string = "keyToString"
let suffix_to_array = "toArray"

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

let get_expression_from_payload payload =
  match payload with
  | PStr [ { pstr_desc } ] -> (
      match pstr_desc with
      | Pstr_eval (expr, _) -> expr
      | _ -> fail loc "Expected expression as attribute payload")
  | _ -> fail loc "Expected expression as attribute payload"

let parse_attribute { attr_name = { Location.txt }; attr_payload } :
    attribute_kind option =
  if txt = mk_attr_with_suffix attribute_name suffix_key_of then
    Some (KeyOf (suffix_key_of, attr_payload))
  else if txt = mk_attr_with_suffix attribute_name suffix_set_type then
    Some (SetType (suffix_set_type, attr_payload))
  else if txt = mk_attr_with_suffix attribute_name suffix_to_generic then
    Some (ToGeneric (suffix_to_generic, attr_payload))
  else if txt = mk_attr_with_suffix attribute_name suffix_partial then
    Some (Partial (suffix_partial, attr_payload))
  else if txt = mk_attr_with_suffix attribute_name suffix_pick then
    Some (Pick (suffix_pick, attr_payload))
  else if txt = mk_attr_with_suffix attribute_name suffix_omit then
    Some (Omit (suffix_omit, attr_payload))
  else if txt = mk_attr_with_suffix attribute_name suffix_to_array then
    Some (ToArray (suffix_to_array, attr_payload))
  else None

let parse_extension { ptype_name; ptype_manifest; ptype_attributes } :
    extension_kind option =
  match ptype_manifest with
  | Some { ptyp_desc = Ptyp_extension ({ Location.txt }, payload) } ->
      (* type identifier in extension payload *)
      let type_labels =
        match get_expression_from_payload payload with
        | { pexp_desc = Pexp_ident lid } -> Longident.flatten_exn lid.txt
        (* %ppx_ts.setType(t, string) *)
        | {
         pexp_desc =
           Pexp_tuple
             [ { pexp_desc = Pexp_ident lid }; { pexp_desc = Pexp_ident _ } ];
        } ->
            Longident.flatten_exn lid.txt
        | _ -> fail Location.none "Missing type identifier"
      in

      if txt = mk_attr_with_suffix attribute_name suffix_key_of then
        Some (KeyOf (ptype_name.txt, type_labels, payload, ptype_attributes))
      else if txt = mk_attr_with_suffix attribute_name suffix_set_type then
        Some (SetType (ptype_name.txt, type_labels, payload, ptype_attributes))
      else if
        txt = mk_attr_with_suffix attribute_name suffix_set_type_except_bool
      then
        Some
          (SetTypeExceptBool
             (ptype_name.txt, type_labels, payload, ptype_attributes))
      else if txt = mk_attr_with_suffix attribute_name suffix_to_generic then
        Some
          (ToGeneric (ptype_name.txt, type_labels, payload, ptype_attributes))
      else if txt = mk_attr_with_suffix attribute_name suffix_partial then
        Some (Partial (ptype_name.txt, type_labels, payload, ptype_attributes))
      else if txt = mk_attr_with_suffix attribute_name suffix_pick then
        Some (Pick (ptype_name.txt, type_labels, payload, ptype_attributes))
      else if txt = mk_attr_with_suffix attribute_name suffix_omit then
        Some (Omit (ptype_name.txt, type_labels, payload, ptype_attributes))
      else None
  | _ -> None

(* get type declaration by label inside module or top-level pstr *)
let rec get_type_decl_from_str_by_labels structure labels =
  let matched_decls =
    match labels with
    | [] ->
        fail Location.none
          "Can not find type declaration with given type identifier"
    | [ label ] ->
        structure
        |> List.filter_map (fun { pstr_desc } ->
               match pstr_desc with
               | Pstr_type (_, decls) -> (
                   let matched_decl =
                     decls
                     |> List.filter (fun { ptype_name = { Location.txt } } ->
                            txt = label)
                   in
                   match matched_decl with
                   | [] -> None
                   | [ decl ] | decl :: _ -> Some decl)
               | _ -> None)
    | label :: labels ->
        structure
        |> List.filter_map (fun { pstr_desc } ->
               match pstr_desc with
               (* module Foo = ... *)
               | Pstr_module
                   {
                     pmb_name;
                     pmb_expr = { pmod_desc = Pmod_structure structure };
                   } ->
                   if pmb_name.txt = Some label then
                     get_type_decl_from_str_by_labels structure labels
                   else None
               (* module Foo: Foo = ... *)
               | Pstr_module
                   {
                     pmb_name;
                     pmb_expr =
                       {
                         pmod_desc =
                           Pmod_constraint
                             ({ pmod_desc = Pmod_structure structure }, _);
                       };
                   } ->
                   if pmb_name.txt = Some label then
                     get_type_decl_from_str_by_labels structure labels
                   else None
               | _ -> None)
  in
  match matched_decls with [] -> None | [ decl ] | decl :: _ -> Some decl

(* get type declaration by label inside module or top-level  *)
let rec get_type_decl_from_sig_by_labels signature labels =
  let matched_decls =
    match labels with
    | [] ->
        fail Location.none
          "Can not find type declaration with given type identifier"
    | [ label ] ->
        signature
        |> List.filter_map (fun { psig_desc } ->
               match psig_desc with
               | Psig_type (_, decls) -> (
                   let matched_decl =
                     decls
                     |> List.filter (fun { ptype_name = { Location.txt } } ->
                            txt = label)
                   in
                   match matched_decl with
                   | [] -> None
                   | [ decl ] | decl :: _ -> Some decl)
               | _ -> None)
    | label :: labels ->
        signature
        |> List.filter_map (fun { psig_desc } ->
               match psig_desc with
               | Psig_modtype
                   {
                     pmtd_name;
                     pmtd_type = Some { pmty_desc = Pmty_signature signature };
                   } ->
                   if pmtd_name.txt = label then
                     get_type_decl_from_sig_by_labels signature labels
                   else None
               | _ -> None)
  in
  match matched_decls with [] -> None | [ decl ] | decl :: _ -> Some decl

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
let make_label_decls_with_core_type ?(except_bool = false) ?(is_option = false)
    decls core_type =
  decls
  |> List.map (fun { pld_name; pld_loc; pld_type } ->
         let core_type =
           match (except_bool, pld_type) with
           | ( true,
               { ptyp_desc = Ptyp_constr ({ Location.txt = Lident txt }, _) } )
             -> (
               match txt with "bool" -> pld_type | _ -> core_type)
           | _ -> core_type
         in
         Type.field ~loc:pld_loc pld_name
           (match is_option with
           | true -> Typ.constr (lid "option") [ core_type ]
           | false -> core_type))

(* make label_declaration with labels *)
let make_label_decls_with_labels ?(is_option = false) ?(is_omit = false) decls
    labels =
  let matched_decls =
    if is_omit then
      decls
      |> List.filter (fun { pld_name = { Location.txt } } ->
             labels |> List.exists (fun label -> label <> txt))
    else
      decls
      |> List.filter (fun { pld_name = { Location.txt } } ->
             labels |> List.exists (fun label -> label = txt))
  in
  matched_decls
  |> List.map (fun { pld_name; pld_loc; pld_type } ->
         Type.field ~loc:pld_loc pld_name
           (match is_option with
           | true -> Typ.constr (lid "option") [ pld_type ]
           | false -> pld_type))

(* make label_declaration with is_option flag *)
let make_label_decls ?(is_option = false) decls =
  decls
  |> List.map (fun { pld_name; pld_type; pld_loc } ->
         Type.field ~loc:pld_loc pld_name
           (match is_option with
           | true -> Typ.constr (lid "option") [ pld_type ]
           | false -> pld_type))
