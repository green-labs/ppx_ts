open Ppxlib
open Parsetree
open Ast_helper
open Utils

(* keyOf attribute mapper *)
let make_signature_items name loc manifest kind suffix =
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
          Sig.value
            (Val.mk
               (mkloc (name ^ "_" ^ suffix_key_to_string) loc)
               (Typ.arrow Nolabel
                  (Typ.constr (Utils.lid (name ^ "_" ^ suffix)) [])
                  (Typ.constr (Utils.lid "string") [])));
        ]
      in
      decls
  | _ -> fail loc "This type is not handled by ppx_ts"

(* keyOf extension mapper *)
let make_new_signature_item name loc manifest kind attributes =
  match (manifest, kind) with
  | None, Ptype_abstract -> fail loc "Can't handle the unspecified type"
  | None, Ptype_record decls ->
      let keys = decls |> List.map (fun { pld_name = { txt } } -> txt) in
      Sig.type_ Recursive
        [
          Type.mk (mkloc name loc) ~attrs:attributes
            ~kind:(Ptype_variant (make_const_decls keys loc));
        ]
  | _ -> fail loc "This type is not handled by ppx_ts"

(* keyOf toString fn *)
let make_signature_item name loc manifest kind =
  match (manifest, kind) with
  | None, Ptype_abstract -> fail loc "Can't handle the unspecified type"
  | None, Ptype_record _ ->
      Sig.value
        (Val.mk
           (mkloc (name ^ "_" ^ suffix_key_to_string) loc)
           (Typ.arrow Nolabel
              (Typ.constr (Utils.lid name) [])
              (Typ.constr (Utils.lid "string") [])))
  | _ -> fail loc "This type is not handled by ppx_ts"
