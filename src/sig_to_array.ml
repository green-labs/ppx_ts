open Ppxlib
open Parsetree
open Ast_helper
open Utils

(* keyOf attribute mapper *)
let make_signature_items name loc manifest kind suffix =
  let value_binding =
    [
      Sig.value ~loc
        (Val.mk
           (mkloc (name ^ "_" ^ suffix) loc)
           (Typ.constr (Utils.lid "array")
              [ Typ.constr (Utils.lid "string") [] ]));
    ]
  in

  match (manifest, kind) with
  | Some { ptyp_desc = Ptyp_variant (_, _, _) }, Ptype_abstract
  | None, Ptype_variant _ ->
      value_binding
  | _ -> fail loc "This type is not handled by ppx_ts"

(* keyOf extension mapper *)
let make_new_signature_item name loc manifest kind attributes =
  let value_description =
    Sig.value ~loc
      (Val.mk ~loc ~attrs:attributes (mkloc name loc)
         (Typ.constr (Utils.lid "array") [ Typ.constr (Utils.lid "string") [] ]))
  in

  match (manifest, kind) with
  | Some { ptyp_desc = Ptyp_variant (_, _, _) }, Ptype_abstract
  | None, Ptype_record _ ->
      value_description
  | _ -> fail loc "This type is not handled by ppx_ts"
