(*
 * This file has been generated by the OCamlClientCodegen generator for openapi-generator.
 *
 * Generated by: https://openapi-generator.tech
 *
 *)

type t = {
    index: int32 option [@default None];
    message: Chat_completion_response_message.t;
    finish_reason: string option [@default None];
} [@@deriving yojson { strict = false }, show ];;
