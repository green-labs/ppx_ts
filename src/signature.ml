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
let make_label_decls_with_core_type ?(is_option = false) decls core_type =
  decls
  |> List.map (fun { pld_name; pld_loc } ->
         Type.field ~loc:pld_loc pld_name
           (match is_option with
           | true -> Typ.constr (Utils.lid "option") [ core_type ]
           | false -> core_type))

(* make label_declaration with is_option flag *)
let make_label_decls ?(is_option = false) decls =
  decls
  |> List.map (fun { pld_name; pld_type; pld_loc } ->
         Type.field ~loc:pld_loc pld_name
           (match is_option with
           | true -> Typ.constr (Utils.lid "option") [ pld_type ]
           | false -> pld_type))

(* keyOf attribute mapper *)
let make_signature_item_key_of name loc manifest kind suffix =
  match (manifest, kind) with
  | None, Ptype_abstract -> fail loc "Can't handle the unspecified type"
  | None, Ptype_record decls ->
      let keys = decls |> List.map (fun { pld_name = { txt } } -> txt) in
      let decls =
        [
          Sig.type_ Recursive
            [
              Type.mk
                (mkloc (name ^ "_" ^ suffix) loc)
                ~kind:(Ptype_variant (make_const_decls keys loc));
            ];
          Sig.type_ Recursive
            [
              Type.mk
                (mkloc (name ^ "_" ^ suffix_key_to_string) loc)
                ?manifest:
                  (Some
                     (Typ.arrow Nolabel
                        (Typ.constr (Utils.lid (name ^ "_" ^ suffix)) [])
                        (Typ.constr (Utils.lid "string") [])));
            ];
        ]
      in
      decls
  | _ -> fail loc "This type is not handled by ppx_ts"

(* setType attribute mapper *)
let make_signature_item_set_type name loc manifest kind suffix payload =
  match (manifest, kind, payload) with
  | None, Ptype_abstract, _ -> fail loc "Can't handle the unspecified type"
  | ( None,
      Ptype_record decls,
      PStr [ { pstr_desc = Pstr_eval ({ pexp_desc = Pexp_ident lid }, _) } ] )
    ->
      let new_pld_type = Typ.constr lid [] in
      let decls =
        [
          Sig.type_ Recursive
            [
              Type.mk
                (mkloc (name ^ "_" ^ suffix) loc)
                ~priv:Public
                ~kind:
                  (Ptype_record
                     (make_label_decls_with_core_type decls new_pld_type));
            ];
        ]
      in
      decls
  | _ -> fail loc "This type is not handled by ppx_ts"

(* toGeneric attribute mapper *)
let make_signature_item_to_generic name loc manifest kind suffix =
  match (manifest, kind) with
  | None, Ptype_abstract -> fail loc "Can't handle the unspecified type"
  | None, Ptype_record decls ->
      (* type param label *)
      let type_param = Typ.var "a" in
      let decls =
        [
          Sig.type_ Recursive
            [
              Type.mk
                (mkloc (name ^ "_" ^ suffix) loc)
                ~priv:Public
                ~params:[ (type_param, (NoVariance, NoInjectivity)) ]
                ~kind:
                  (Ptype_record
                     (make_label_decls_with_core_type decls type_param));
            ];
        ]
      in
      decls
  | _ -> fail loc "This type is not handled by ppx_ts"

(* partial attribute mapper *)
let make_signature_item_partial name loc manifest kind suffix =
  match (manifest, kind) with
  (* type t *)
  | None, Ptype_abstract -> fail loc "Can't handle the unspecified type"
  | None, Ptype_record decls ->
      let decls =
        [
          Sig.type_ Recursive
            [
              Type.mk
                (mkloc (name ^ "_" ^ suffix) loc)
                ~priv:Public
                ~kind:(Ptype_record (make_label_decls ~is_option:true decls));
            ];
        ]
      in
      decls
  | _ -> fail loc "This type is not handled by @ppx_ts.toGeneric"

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
             make_signature_item_key_of type_name ptype_loc ptype_manifest
               ptype_kind suffix
         | Some (SetType (suffix, payload)) ->
             make_signature_item_set_type type_name ptype_loc ptype_manifest
               ptype_kind suffix payload
         | Some (ToGeneric (suffix, _)) ->
             make_signature_item_to_generic type_name ptype_loc ptype_manifest
               ptype_kind suffix
         | Some (Partial (suffix, _)) ->
             make_signature_item_partial type_name ptype_loc ptype_manifest
               ptype_kind suffix
         | None -> [])
  |> List.concat

let map_signature_item mapper ({ psig_desc } as signature_item) =
  match psig_desc with
  | Psig_type (_, decls) ->
      let signature_items = decls |> List.map map_type_decl |> List.concat in
      mapper#signature_item signature_item :: signature_items
  | _ -> [ mapper#signature_item signature_item ]
