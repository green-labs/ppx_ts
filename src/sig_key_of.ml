open Ppxlib
open Parsetree
open Ast_helper
open Utils

(* keyOf attribute mapper *)
let make_signature_item name loc manifest kind suffix =
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
