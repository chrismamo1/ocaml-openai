(*
 * This file has been generated by the OCamlClientCodegen generator for openapi-generator.
 *
 * Generated by: https://openapi-generator.tech
 *
 *)

type t = {
    id: string;
    _object: string [@key "object"];
    bytes: int32;
    created_at: int32;
    filename: string;
    purpose: string;
    status: string option [@default None];
    status_details: Yojson.Safe.t option [@default None];
} [@@deriving yojson { strict = false }, show, make ];;
