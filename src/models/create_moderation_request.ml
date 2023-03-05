(*
 * This file has been generated by the OCamlClientCodegen generator for openapi-generator.
 *
 * Generated by: https://openapi-generator.tech
 *
 *)

type t = {
    (* The input text to classify *)
    input: One_ofstringarray.t;
    (* Two content moderations models are available: `text-moderation-stable` and `text-moderation-latest`.  The default is `text-moderation-latest` which will be automatically upgraded over time. This ensures you are always using our most accurate model. If you use `text-moderation-stable`, we will provide advanced notice before updating the model. Accuracy of `text-moderation-stable` may be slightly lower than for `text-moderation-latest`.  *)
    model: string option [@default None];
} [@@deriving yojson { strict = false }, show ];;

let create (input : One_ofstringarray.t) : t = {
    input = input;
    model = None;
}

