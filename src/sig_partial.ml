open Ppxlib
open Parsetree
open Ast_helper
open Utils

(* partial attribute mapper *)
let make_signature_item name loc manifest kind suffix =
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
