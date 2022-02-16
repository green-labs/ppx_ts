open Ppxlib
open Parsetree
open Ast_helper
open Utils

(* make constructor declaration with label *)
let make_const_decls labels loc =
  labels
  |> List.map (fun label -> String.capitalize_ascii label)
  |> List.map (fun label -> Type.constructor ~loc (mkloc label loc))

(* make label_declaration with new type constructor using lid *)
let make_label_decls_with_lid ?(is_option = false) labels loc lid =
  let core_type_list =
    match is_option with
    | true -> [ Typ.constr (Utils.lid "option") [] ]
    | false -> []
  in
  labels
  |> List.map (fun label ->
         Type.field ~loc (mkloc label loc) (Typ.constr lid core_type_list))

(* make label_declaration with is_option flag *)
let make_label_decls ?(is_option = false) decls =
  decls
  |> List.map (fun { pld_name; pld_type; pld_loc } ->
         Type.field ~loc:pld_loc pld_name
           (match is_option with
           | true -> Typ.constr (Utils.lid "option") [ pld_type ]
           | false -> pld_type))

(* keyOf attribute mapper *)
let make_structure_item_key_of name loc manifest kind suffix =
  match (manifest, kind) with
  (* type t *)
  | None, Ptype_abstract -> fail loc "Can't handle the unspecified type"
  | None, Ptype_record decls ->
      let keys = decls |> List.map (fun { pld_name = { txt } } -> txt) in
      let decls =
        [
          Str.type_ Recursive
            [
              Type.mk
                (mkloc (name ^ "_" ^ suffix) loc)
                ~priv:Public
                ~kind:(Ptype_variant (make_const_decls keys loc));
            ];
        ]
      in
      decls
  | _ -> fail loc "This type is not handled by @ppx_ts.keyOf"

(* setType attribute mapper *)
let make_structure_item_set_type name loc manifest kind suffix payload =
  match (manifest, kind, payload) with
  (* type t *)
  | None, Ptype_abstract, _ -> fail loc "Can't handle the unspecified type"
  | ( None,
      Ptype_record decls,
      PStr [ { pstr_desc = Pstr_eval ({ pexp_desc = Pexp_ident lid }, _) } ] )
    ->
      let keys = decls |> List.map (fun { pld_name = { txt } } -> txt) in
      let decls =
        [
          Str.type_ Recursive
            [
              Type.mk
                (mkloc (name ^ "_" ^ suffix) loc)
                ~priv:Public
                ~kind:(Ptype_record (make_label_decls_with_lid keys loc lid));
            ];
        ]
      in
      decls
  | _ -> fail loc "This type is not handled by @ppx_ts.setType"

(* toGeneric attribute mapper *)
let make_structure_item_to_generic name loc manifest kind suffix =
  match (manifest, kind) with
  (* type t *)
  | None, Ptype_abstract -> fail loc "Can't handle the unspecified type"
  | None, Ptype_record decls ->
      let keys = decls |> List.map (fun { pld_name = { txt } } -> txt) in
      let decls =
        [
          Str.type_ Recursive
            [
              Type.mk
                (mkloc (name ^ "_" ^ suffix) loc)
                ~priv:Public
                ~kind:(Ptype_variant (make_const_decls keys loc));
            ];
        ]
      in
      decls
  | _ -> fail loc "This type is not handled by @ppx_ts.toGeneric"

(* partial attribute mapper *)
let make_structure_item_partial name loc manifest kind suffix =
  match (manifest, kind) with
  (* type t *)
  | None, Ptype_abstract -> fail loc "Can't handle the unspecified type"
  | None, Ptype_record decls ->
      let decls =
        [
          Str.type_ Recursive
            [
              Type.mk
                (mkloc (name ^ "_" ^ suffix) loc)
                ~priv:Public
                ~kind:(Ptype_record (make_label_decls ~is_option:true decls));
            ];
        ]
      in
      decls
  | _ -> fail loc "This type is not handled by @ppx_ts.partial"

let map_type_decl decl =
  let {
    ptype_attributes;
    ptype_name = { txt = type_name };
    ptype_manifest;
    ptype_loc;
    ptype_kind;
  } =
    decl
  in
  (* attributes -> structure_item list list -> structure_item list *)
  ptype_attributes |> List.map parse_attribute
  |> List.map (fun attribute ->
         match attribute with
         | Some (KeyOf (suffix, _)) ->
             make_structure_item_key_of type_name ptype_loc ptype_manifest
               ptype_kind suffix
         | Some (SetType (suffix, payload)) ->
             make_structure_item_set_type type_name ptype_loc ptype_manifest
               ptype_kind suffix payload
         | Some (ToGeneric (suffix, _)) ->
             make_structure_item_to_generic type_name ptype_loc ptype_manifest
               ptype_kind suffix
         | Some (Partial (suffix, _)) ->
             make_structure_item_partial type_name ptype_loc ptype_manifest
               ptype_kind suffix
         | None -> [])
  |> List.concat

let map_structure_item mapper ({ pstr_desc } as structure_item) =
  match pstr_desc with
  | Pstr_type (_, decls) ->
      let structure_items = decls |> List.map map_type_decl |> List.concat in
      mapper#structure_item structure_item :: structure_items
  | _ -> [ mapper#structure_item structure_item ]
