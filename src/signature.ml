open Ppxlib
open Parsetree
open Ast_helper
open Utils

let make_const_decls labels loc =
  labels
  |> List.map (fun label -> String.uppercase_ascii label)
  |> List.map (fun label -> Type.constructor ~loc (mkloc label loc))

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

  let _attributes =
    get_attribute_by_name ptype_attributes
      (attribute_name ^ attribute_name_keyof)
  in

  match (ptype_manifest, ptype_kind) with
  | None, Ptype_abstract -> fail ptype_loc "Can't handle the unspecified type"
  | None, Ptype_record decls ->
      let keys = decls |> List.map (fun { pld_name = { txt } } -> txt) in
      let decls =
        [
          Sig.type_ Recursive
            [
              Type.mk
                (mkloc (type_name ^ "_keyof") ptype_loc)
                ~priv:Public
                ~kind:(Ptype_variant (make_const_decls keys ptype_loc));
            ];
        ]
      in
      decls
  | _ -> fail ptype_loc "This type is not handled by ppx_ts"

let map_signature_item mapper ({ psig_desc } as signature_item) =
  match psig_desc with
  | Psig_type (_, decls) ->
      let signature_items = decls |> List.map map_type_decl |> List.concat in
      mapper#signature_item signature_item :: signature_items
  | _ -> [ mapper#signature_item signature_item ]
