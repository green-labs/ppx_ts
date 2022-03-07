open Ppxlib
open Parsetree
open Ast_helper
open Utils

(* toGeneric attribute mapper *)
let make_signature_items name loc manifest kind suffix =
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

(* toGeneric extension mapper *)
let make_signature_item name loc manifest kind attributes =
  match (manifest, kind) with
  | None, Ptype_abstract -> fail loc "Can't handle the unspecified type"
  | None, Ptype_record decls ->
      (* type param label *)
      let type_param = Typ.var "a" in
      Sig.type_ Recursive
        [
          Type.mk (mkloc name loc) ~priv:Public ~attrs:attributes
            ~params:[ (type_param, (NoVariance, NoInjectivity)) ]
            ~kind:
              (Ptype_record (make_label_decls_with_core_type decls type_param));
        ]
  | _ -> fail loc "This type is not handled by ppx_ts"
