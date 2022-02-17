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
  |> List.map (fun attribute ->
         match attribute with
         | Some (KeyOf (suffix, _)) ->
             Str_key_of.make_structure_item type_name ptype_loc ptype_manifest
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
         | None -> [])
  |> List.concat

let map_structure_item mapper ({ pstr_desc } as structure_item) =
  match pstr_desc with
  | Pstr_type (_, decls) ->
      let structure_items = decls |> List.map map_type_decl |> List.concat in
      mapper#structure_item structure_item :: structure_items
  | _ -> [ mapper#structure_item structure_item ]
