(*
 * This file has been generated by the OCamlClientCodegen generator for openapi-generator.
 *
 * Generated by: https://openapi-generator.tech
 *
 *)

type t = {
    (* Input text to get embeddings for, encoded as a string or array of tokens. To get embeddings for multiple inputs in a single request, pass an array of strings or array of token arrays. Each input must not exceed 8192 tokens in length.  *)
    input: One_ofstringarrayarrayarray.t;
} [@@deriving yojson { strict = false }, show ];;

let create (input : One_ofstringarrayarrayarray.t) : t = {
    input = input;
}

