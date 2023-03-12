(*
 * This file has been generated by the OCamlClientCodegen generator for openapi-generator.
 *
 * Generated by: https://openapi-generator.tech
 *
 *)

type t = {
  (* Query to search against the documents. *)
  query : string;
  (* Up to 200 documents to search over, provided as a list of strings.  The maximum document length (in tokens) is 2034 minus the number of tokens in the query.  You should specify either `documents` or a `file`, but not both.  *)
  documents : string list;
  (* The ID of an uploaded file that contains documents to search over.  You should specify either `documents` or a `file`, but not both.  *)
  file : string option; [@default None]
  (* The maximum number of documents to be re-ranked and returned by search.  This flag only takes effect when `file` is set.  *)
  max_rerank : int32 option; [@default None]
  (* A special boolean flag for showing metadata. If set to `true`, each document entry in the returned JSON will contain a \''metadata\'' field.  This flag only takes effect when `file` is set.  *)
  return_metadata : bool option; [@default None]
}
[@@deriving yojson { strict = false }, show, make]
