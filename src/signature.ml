open Ppxlib
open Parsetree
open Utils

let map_type_decl decl =
  let {
    ptype_attributes;
    ptype_name = { txt = type_name };
    ptype_manifest;
    ptype_loc;
    ptype_kind;
  } =
    decl
  in
  (* attributes -> structure_item list list -> structure_item list *)
  ptype_attributes |> List.map parse_attribute
  |> List.map (fun (attribute : attribute_kind option) ->
         match attribute with
         | Some (KeyOf (suffix, _)) ->
             Sig_key_of.make_signature_items type_name ptype_loc ptype_manifest
               ptype_kind suffix
         | Some (SetType (suffix, payload)) ->
             Sig_set_type.make_signature_items type_name ptype_loc
               ptype_manifest ptype_kind suffix payload
         | Some (ToGeneric (suffix, _)) ->
             Sig_to_generic.make_signature_items type_name ptype_loc
               ptype_manifest ptype_kind suffix
         | Some (Partial (suffix, _)) ->
             Sig_partial.make_signature_items type_name ptype_loc ptype_manifest
               ptype_kind suffix
         | Some (Pick (suffix, payload)) ->
             Sig_pick.make_signature_item type_name ptype_loc ptype_manifest
               ptype_kind suffix payload
         | Some (Omit (suffix, payload)) ->
             Sig_omit.make_signature_item type_name ptype_loc ptype_manifest
               ptype_kind suffix payload
         | None -> [])
  |> List.concat

let map_signature_item mapper signature ({ psig_desc } as signature_item) =
  match psig_desc with
  | Psig_type (_, decls) -> (
      let extension =
        match decls |> List.filter_map parse_extension with
        | [] -> None
        | [ kind ] | kind :: _ -> Some kind
      in

      match extension with
      | Some (KeyOf (type_name, type_labels, _, attributes)) -> (
          match get_type_decl_from_sig_by_labels signature type_labels with
          | Some { ptype_loc; ptype_manifest; ptype_kind } ->
              let new_signature_item =
                Sig_key_of.make_new_signature_item type_name ptype_loc
                  ptype_manifest ptype_kind attributes
              in
              let to_string_signature_item =
                Sig_key_of.make_signature_item type_name ptype_loc
                  ptype_manifest ptype_kind
              in
              [
                mapper#signature_item new_signature_item;
                to_string_signature_item;
              ]
          | _ -> fail Location.none "Can not find the matching type")
      | Some (SetType (type_name, type_labels, payload, attributes)) -> (
          match get_type_decl_from_sig_by_labels signature type_labels with
          | Some { ptype_loc; ptype_manifest; ptype_kind } ->
              let new_signature_item =
                Sig_set_type.make_new_signature_item type_name ptype_loc
                  ptype_manifest ptype_kind payload attributes
              in
              [ mapper#signature_item new_signature_item ]
          | _ -> fail Location.none "Can not find the matching type")
      | Some (SetTypeExceptBool (type_name, type_labels, payload, attributes))
        -> (
          match get_type_decl_from_sig_by_labels signature type_labels with
          | Some { ptype_loc; ptype_manifest; ptype_kind } ->
              let new_signature_item =
                Sig_set_type.make_new_signature_item ~except_bool:true type_name
                  ptype_loc ptype_manifest ptype_kind payload attributes
              in
              [ mapper#signature_item new_signature_item ]
          | _ -> fail Location.none "Can not find the matching type")
      | Some (ToGeneric (type_name, type_labels, _, attributes)) -> (
          match get_type_decl_from_sig_by_labels signature type_labels with
          | Some { ptype_loc; ptype_manifest; ptype_kind } ->
              let new_signature_item =
                Sig_to_generic.make_signature_item type_name ptype_loc
                  ptype_manifest ptype_kind attributes
              in
              [ mapper#signature_item new_signature_item ]
          | _ -> fail Location.none "Can not find the matching type")
      | Some (Partial (type_name, type_labels, _, attributes)) -> (
          match get_type_decl_from_sig_by_labels signature type_labels with
          | Some { ptype_loc; ptype_manifest; ptype_kind } ->
              let new_signature_item =
                Sig_partial.make_signature_item type_name ptype_loc
                  ptype_manifest ptype_kind attributes
              in
              [ mapper#signature_item new_signature_item ]
          | _ -> fail Location.none "Can not find the matching type")
      | _ ->
          let signature_items =
            decls |> List.map map_type_decl |> List.concat
          in
          mapper#signature_item signature_item :: signature_items)
  | _ -> [ mapper#signature_item signature_item ]
