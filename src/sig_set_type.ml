open Ppxlib
open Parsetree
open Ast_helper
open Utils

(* setType attribute mapper *)
let make_signature_items name loc manifest kind suffix payload =
  match (manifest, kind, payload) with
  | None, Ptype_abstract, _ -> fail loc "Can't handle the unspecified type"
  | ( None,
      Ptype_record decls,
      PStr [ { pstr_desc = Pstr_eval ({ pexp_desc = Pexp_ident lid }, _) } ] )
    ->
      let new_pld_type = Typ.constr lid [] in
      let decls =
        [
          Sig.type_ Recursive
            [
              Type.mk
                (mkloc (name ^ "_" ^ suffix) loc)
                ~priv:Public
                ~kind:
                  (Ptype_record
                     (make_label_decls_with_core_type decls new_pld_type));
            ];
        ]
      in
      decls
  | _ -> fail loc "This type is not handled by ppx_ts"

(* setType extension mapper *)
let make_new_signature_item ?(except_bool = false) name loc manifest kind
    payload attributes =
  match (manifest, kind, payload) with
  | None, Ptype_abstract, _ -> fail loc "Can't handle the unspecified type"
  | ( None,
      Ptype_record decls,
      PStr
        [
          {
            pstr_desc =
              Pstr_eval
                ( {
                    pexp_desc =
                      Pexp_tuple
                        [
                          { pexp_desc = Pexp_ident _ };
                          { pexp_desc = Pexp_ident lid };
                        ];
                  },
                  _ );
          };
        ] ) ->
      let new_pld_type = Typ.constr lid [] in
      Sig.type_ Recursive
        [
          Type.mk (mkloc name loc) ~priv:Public ~attrs:attributes
            ~kind:
              (Ptype_record
                 (make_label_decls_with_core_type decls ~except_bool
                    new_pld_type));
        ]
  | _ -> fail loc "This type is not handled by @ppx_ts.setType"
