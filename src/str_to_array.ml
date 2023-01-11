open Ppxlib
open Parsetree
open Ast_helper
open Utils

(* toArray attribute mapper *)
let make_structure_items name loc manifest kind suffix =
  let keys =
    match (manifest, kind) with
    (* type t *)
    | Some { ptyp_desc = Ptyp_variant (row_fields, _, _) }, Ptype_abstract ->
        row_fields
        |> List.filter_map (fun { prf_desc; prf_loc = loc } ->
               match prf_desc with
               | Rtag ({ txt }, _, _) -> Some (txt, loc)
               | Rinherit _ -> None)
    | None, Ptype_variant cds ->
        cds |> List.map (fun { pcd_name = { txt }; pcd_loc } -> (txt, pcd_loc))
    | _ -> fail loc "This type is not handled by @ppx_ts.toArray"
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

(* toArray extension mapper *)
let make_new_structure_item name loc manifest kind attributes =
  let keys =
    match (manifest, kind) with
    (* type t *)
    | Some { ptyp_desc = Ptyp_variant (row_fields, _, _) }, Ptype_abstract ->
        row_fields
        |> List.filter_map (fun { prf_desc; prf_loc = loc } ->
               match prf_desc with
               | Rtag ({ txt }, _, _) -> Some (txt, loc)
               | Rinherit _ -> None)
    | None, Ptype_variant cds ->
        cds |> List.map (fun { pcd_name = { txt }; pcd_loc } -> (txt, pcd_loc))
    | _ -> fail loc "This type is not handled by %ppx_ts.toArray"
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
