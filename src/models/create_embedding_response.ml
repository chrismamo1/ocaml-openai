(*
 * This file has been generated by the OCamlClientCodegen generator for openapi-generator.
 *
 * Generated by: https://openapi-generator.tech
 *
 *)

type t = {
  _object : string; [@key "object"]
  model : string;
  data : Create_embedding_response_data.t list;
  usage : Create_embedding_response_usage.t;
}
[@@deriving yojson { strict = false }, show, make]
