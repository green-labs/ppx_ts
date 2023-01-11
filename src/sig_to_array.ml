open Ppxlib
open Parsetree
open Ast_helper
open Utils

(* keyOf attribute mapper *)
let make_signature_items name loc manifest kind suffix =
  match (manifest, kind) with
  | None, Ptype_abstract -> fail loc "Can't handle the unspecified type"
  | None, Ptype_variant _ ->
      [
        Sig.value ~loc
          (Val.mk
             (mkloc (name ^ "_" ^ suffix) loc)
             (Typ.constr (Utils.lid "array")
                [ Typ.constr (Utils.lid "string") [] ]));
      ]
  | _ -> fail loc "This type is not handled by ppx_ts"

(* keyOf extension mapper *)
let make_new_signature_item name loc manifest kind attributes =
  match (manifest, kind) with
  | None, Ptype_abstract -> fail loc "Can't handle the unspecified type"
  | None, Ptype_record _ ->
      Sig.value ~loc
        (Val.mk ~loc ~attrs:attributes (mkloc name loc)
           (Typ.constr (Utils.lid "array")
              [ Typ.constr (Utils.lid "string") [] ]))
  | _ -> fail loc "This type is not handled by ppx_ts"
