(*
 * This file has been generated by the OCamlClientCodegen generator for openapi-generator.
 *
 * Generated by: https://openapi-generator.tech
 *
 *)

type t = {
    id: string;
    model: string;
    results: Create_moderation_response_results.t list;
} [@@deriving yojson { strict = false }, show, make ];;
