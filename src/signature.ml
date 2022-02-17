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
             Sig_key_of.make_signature_item type_name ptype_loc ptype_manifest
               ptype_kind suffix
         | Some (SetType (suffix, payload)) ->
             Sig_set_type.make_signature_item type_name ptype_loc ptype_manifest
               ptype_kind suffix payload
         | Some (ToGeneric (suffix, _)) ->
             Sig_to_generic.make_signature_item type_name ptype_loc
               ptype_manifest ptype_kind suffix
         | Some (Partial (suffix, _)) ->
             Sig_partial.make_signature_item type_name ptype_loc ptype_manifest
               ptype_kind suffix
         | Some (Pick (suffix, payload)) ->
             Sig_pick.make_signature_item type_name ptype_loc ptype_manifest
               ptype_kind suffix payload
         | Some (Omit (suffix, payload)) ->
             Sig_omit.make_signature_item type_name ptype_loc ptype_manifest
               ptype_kind suffix payload
         | None -> [])
  |> List.concat

let map_signature_item mapper ({ psig_desc } as signature_item) =
  match psig_desc with
  | Psig_type (_, decls) ->
      let signature_items = decls |> List.map map_type_decl |> List.concat in
      mapper#signature_item signature_item :: signature_items
  | _ -> [ mapper#signature_item signature_item ]
