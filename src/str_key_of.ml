open Ppxlib
open Parsetree
open Ast_helper
open Utils

(* keyOf attribute mapper *)
let make_structure_items name loc manifest kind suffix =
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
          Str.value Nonrecursive
            [
              Vb.mk
                (Pat.var (mknoloc @@ name ^ "_" ^ suffix_key_to_string))
                (Exp.fun_ Nolabel None
                   (Pat.var (mknoloc "key"))
                   (Exp.match_
                      (Exp.ident (Utils.lid "key"))
                      (make_match_case keys)));
            ];
        ]
      in
      decls
  | _ -> fail loc "This type is not handled by @ppx_ts.keyOf"

(* keyOf extension mapper *)
let make_new_structure_item name loc manifest kind =
  match (manifest, kind) with
  (* type t *)
  | None, Ptype_abstract -> fail loc "Can't handle the unspecified type"
  | None, Ptype_record decls ->
      let keys = decls |> List.map (fun { pld_name = { txt } } -> txt) in
      Str.type_ Recursive
        [
          Type.mk (mkloc name loc) ~priv:Public
            ~kind:(Ptype_variant (make_const_decls keys loc));
        ]
  | _ -> fail loc "This type is not handled by %ppx_ts.keyOf"

let make_structure_item name loc manifest kind =
  match (manifest, kind) with
  (* type t *)
  | None, Ptype_abstract -> fail loc "Can't handle the unspecified type"
  | None, Ptype_record decls ->
      let keys = decls |> List.map (fun { pld_name = { txt } } -> txt) in
      Str.value Nonrecursive
        [
          Vb.mk
            (Pat.var (mknoloc @@ name ^ "_" ^ suffix_key_to_string))
            (Exp.fun_ Nolabel None
               (Pat.var (mknoloc "key"))
               (Exp.match_ (Exp.ident (Utils.lid "key")) (make_match_case keys)));
        ]
  | _ -> fail loc "This type is not handled by %ppx_ts.keyOf"
