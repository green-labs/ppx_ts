open Ppxlib
open Parsetree
open Ast_helper
open Utils

let make_const_decls labels loc =
  labels
  |> List.map (fun label -> String.capitalize_ascii label)
  |> List.map (fun label -> Type.constructor ~loc (mkloc label loc))

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
                ~priv:Public
                ~kind:(Ptype_variant (make_const_decls keys loc));
            ];
        ]
      in
      decls
  | _ -> fail loc "This type is not handled by ppx_ts"

(* setType attribute mapper *)
let make_signature_item_set_type name loc manifest kind suffix =
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
                ~priv:Public
                ~kind:(Ptype_variant (make_const_decls keys loc));
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
      let keys = decls |> List.map (fun { pld_name = { txt } } -> txt) in
      let decls =
        [
          Sig.type_ Recursive
            [
              Type.mk
                (mkloc (name ^ "_" ^ suffix) loc)
                ~priv:Public
                ~kind:(Ptype_variant (make_const_decls keys loc));
            ];
        ]
      in
      decls
  | _ -> fail loc "This type is not handled by ppx_ts"

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
         | Some (KeyOf suffix) ->
             make_signature_item_key_of type_name ptype_loc ptype_manifest
               ptype_kind suffix
         | Some (SetType suffix) ->
             make_signature_item_set_type type_name ptype_loc ptype_manifest
               ptype_kind suffix
         | Some (ToGeneric suffix) ->
             make_signature_item_to_generic type_name ptype_loc ptype_manifest
               ptype_kind suffix
         | None -> [])
  |> List.concat

let map_signature_item mapper ({ psig_desc } as signature_item) =
  match psig_desc with
  | Psig_type (_, decls) ->
      let signature_items = decls |> List.map map_type_decl |> List.concat in
      mapper#signature_item signature_item :: signature_items
  | _ -> [ mapper#signature_item signature_item ]
