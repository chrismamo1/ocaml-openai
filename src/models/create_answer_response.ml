(*
 * This file has been generated by the OCamlClientCodegen generator for openapi-generator.
 *
 * Generated by: https://openapi-generator.tech
 *
 *)

type t = {
  _object : string option; [@default None] [@key "object"]
  model : string option; [@default None]
  search_model : string option; [@default None]
  completion : string option; [@default None]
  answers : string list;
  selected_documents : Create_answer_response_selected_documents.t list;
}
[@@deriving yojson { strict = false }, show, make]
