(*
 * This file has been generated by the OCamlClientCodegen generator for openapi-generator.
 *
 * Generated by: https://openapi-generator.tech
 *
 *)

type t = {
  prompt_tokens : int32;
  completion_tokens : int32;
  total_tokens : int32;
}
[@@deriving yojson { strict = false }, show, make]
