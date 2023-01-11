open Ppxlib
open Parsetree
open Ast_helper
open Utils

(* toArray attribute mapper *)
let make_structure_items name loc manifest kind suffix =
  match (manifest, kind) with
  (* type t *)
  | None, Ptype_abstract -> fail loc "Can't handle the unspecified type"
  | None, Ptype_variant cds ->
      let keys =
        cds |> List.map (fun { pcd_name = { txt }; pcd_loc } -> (txt, pcd_loc))
      in

      [
        Str.value ~loc Nonrecursive
          [
            Vb.mk
              (Pat.var (mkloc (name ^ "_" ^ suffix) loc))
              (Exp.array
                 (keys
                 |> List.map (fun (key, loc) ->
                        Exp.constant (Pconst_string (key, loc, None)))));
          ];
      ]
  | _ -> fail loc "This type is not handled by @ppx_ts.toArray"

(* toArray extension mapper *)
let make_new_structure_item name loc manifest kind attributes =
  match (manifest, kind) with
  (* type t *)
  | None, Ptype_abstract -> fail loc "Can't handle the unspecified type"
  | None, Ptype_variant cds ->
      let keys =
        cds |> List.map (fun { pcd_name = { txt }; pcd_loc } -> (txt, pcd_loc))
      in
      Str.value ~loc Nonrecursive
        [
          Vb.mk ~loc ~attrs:attributes
            (Pat.var (mkloc name loc))
            (Exp.array
               (keys
               |> List.map (fun (key, loc) ->
                      Exp.constant (Pconst_string (key, loc, None)))));
        ]
  | _ -> fail loc "This type is not handled by %ppx_ts.toArray"
