open Ppxlib
open Parsetree
open Ast_helper
open Utils

(* omit attribute mapper *)
let make_structure_item name loc manifest kind suffix payload =
  match (manifest, kind) with
  (* type t *)
  | None, Ptype_abstract -> fail loc "Can't handle the unspecified type"
  | None, Ptype_record decls -> (
      let { pexp_desc } = get_expression_from_payload payload in
      match pexp_desc with
      | Pexp_array expressions -> (
          let payload_labels =
            expressions
            |> List.map (fun { pexp_desc } ->
                   match pexp_desc with
                   | Pexp_constant (Pconst_string (label, _, _)) -> label
                   | _ -> fail loc "Record field name should be a string")
            |> List.fold_left
                 (fun acc label ->
                   if acc |> List.exists (fun a -> a = label) then acc
                   else label :: acc)
                 []
          in
          let decl_labels =
            decls |> List.map (fun { pld_name = { Location.txt } } -> txt)
          in

          (* TODO improve error loc *)
          (* Check whether labels in payload are matched to labels in decls *)
          let is_matched =
            payload_labels
            |> List.for_all (fun label ->
                   decl_labels
                   |> List.exists (fun decl_label -> decl_label = label))
          in
          let is_valid =
            payload_labels |> List.length < (decl_labels |> List.length)
          in

          match (is_matched, is_valid) with
          | false, false ->
              fail loc
                "Label are not matched to keys in Record, At least one label \
                 needs to be remained"
          | false, true -> fail loc "Label are not matched to keys in Record"
          | true, false -> fail loc "At least one label needs to be remained"
          | true, true ->
              let suffix_distinct_labels =
                payload_labels
                |> List.fold_left (fun suffix label -> suffix ^ "_" ^ label) ""
              in

              let decls =
                [
                  Str.type_ Recursive
                    [
                      Type.mk
                        (mkloc
                           (name ^ "_" ^ suffix ^ suffix_distinct_labels)
                           loc)
                        ~priv:Public
                        ~kind:
                          (Ptype_record
                             (make_label_decls_with_labels ~is_omit:true decls
                                payload_labels));
                    ];
                ]
              in
              decls)
      | _ -> fail loc "@ppx_ts.pick payload should be string array")
  | _ -> fail loc "This type is not handled by @ppx_ts.pick"
