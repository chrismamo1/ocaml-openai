(*
 * This file has been generated by the OCamlClientCodegen generator for openapi-generator.
 *
 * Generated by: https://openapi-generator.tech
 *
 *)

type t = {
  _object : string; [@key "object"]
  created : int32;
  choices : Create_completion_response_choices.t list;
  usage : Create_completion_response_usage.t;
}
[@@deriving yojson { strict = false }, show, make]
