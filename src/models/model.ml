(*
 * This file has been generated by the OCamlClientCodegen generator for openapi-generator.
 *
 * Generated by: https://openapi-generator.tech
 *
 *)

type t = {
  id : string;
  _object : string; [@key "object"]
  created : int32;
  owned_by : string;
}
[@@deriving yojson { strict = false }, show, make]
