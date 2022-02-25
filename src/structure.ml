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
             Str_key_of.make_structure_items type_name ptype_loc ptype_manifest
               ptype_kind suffix
         | Some (SetType (suffix, payload)) ->
             Str_set_type.make_structure_item type_name ptype_loc ptype_manifest
               ptype_kind suffix payload
         | Some (ToGeneric (suffix, _)) ->
             Str_to_generic.make_structure_item type_name ptype_loc
               ptype_manifest ptype_kind suffix
         | Some (Partial (suffix, _)) ->
             Str_partial.make_structure_item type_name ptype_loc ptype_manifest
               ptype_kind suffix
         | Some (Pick (suffix, payload)) ->
             Str_pick.make_structure_item type_name ptype_loc ptype_manifest
               ptype_kind suffix payload
         | Some (Omit (suffix, payload)) ->
             Str_omit.make_structure_item type_name ptype_loc ptype_manifest
               ptype_kind suffix payload
         | None -> [])
  |> List.concat

let map_structure_item mapper structure ({ pstr_desc } as structure_item) =
  match pstr_desc with
  | Pstr_type (_, decls) -> (
      let extension =
        match decls |> List.filter_map parse_extension with
        | [] -> None
        | [ kind ] | kind :: _ -> Some kind
      in

      match extension with
      | Some (KeyOf (type_name, type_labels, _payload)) -> (
          match get_type_decl_by_labels structure type_labels with
          | Some { ptype_loc; ptype_manifest; ptype_kind } ->
              let new_structure_item =
                Str_key_of.make_structure_item type_name ptype_loc
                  ptype_manifest ptype_kind
              in
              [ mapper#structure_item new_structure_item ]
          | _ -> fail Location.none "Can not find the matching type")
      | _ ->
          let structure_items =
            decls |> List.map map_type_decl |> List.concat
          in
          mapper#structure_item structure_item :: structure_items)
  | _ -> [ mapper#structure_item structure_item ]
