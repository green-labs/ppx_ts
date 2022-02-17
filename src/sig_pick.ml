open Ppxlib
open Parsetree
open Ast_helper
open Utils

(* pick attribute mapper *)
let make_signature_item name loc manifest kind suffix payload =
  match (manifest, kind) with
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
          in
          let decl_labels =
            decls |> List.map (fun { pld_name = { Location.txt } } -> txt)
          in

          (* Check whether labels in payload are matched to labels in decls *)
          let is_valid =
            payload_labels
            |> List.for_all (fun label ->
                   decl_labels
                   |> List.exists (fun decl_label -> decl_label = label))
          in
          match is_valid with
          | false -> fail loc "Labels are not matched to keys in Record"
          | true ->
              let distinct_labels =
                payload_labels
                |> List.fold_left
                     (fun acc label ->
                       if acc |> List.exists (fun a -> a = label) then acc
                       else label :: acc)
                     []
              in
              let suffix_distinct_labels =
                distinct_labels
                |> List.fold_left (fun suffix label -> suffix ^ "_" ^ label) ""
              in

              let decls =
                [
                  Sig.type_ Recursive
                    [
                      Type.mk
                        (mkloc
                           (name ^ "_" ^ suffix ^ suffix_distinct_labels)
                           loc)
                        ~priv:Public
                        ~kind:
                          (Ptype_record
                             (make_label_decls_with_labels decls distinct_labels));
                    ];
                ]
              in
              decls)
      | _ -> fail loc "@ppx_ts.pick payload should be string array")
  | _ -> fail loc "This type is not handled by @ppx_ts.pick"
