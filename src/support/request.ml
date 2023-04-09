let api_key = Sys.getenv "OPENAI_API_KEY"
let base_url = "https://api.openai.com/v1"

let default_headers =
  Cohttp.Header.add
    (Cohttp.Header.init_with "Content-Type" "application/json")
    "Authorization" ("Bearer " ^ api_key)

let option_fold f default o = match o with Some v -> f v | None -> default
let build_uri operation_path = Uri.of_string (base_url ^ operation_path)
let add_string_header headers key value = Cohttp.Header.add headers key value

let add_string_header_multi headers key values =
  Cohttp.Header.add_multi headers key values

let add_header headers key to_string value =
  Cohttp.Header.add headers key (to_string value)

let add_header_multi headers key to_string value =
  Cohttp.Header.add_multi headers key (to_string value)

let maybe_add_header headers key to_string value =
  option_fold (add_header headers key to_string) headers value

let maybe_add_header_multi headers key to_string value =
  option_fold (add_header_multi headers key to_string) headers value

let write_string_body s = Cohttp_lwt.Body.of_string s

let write_json_body payload =
  Cohttp_lwt.Body.of_string (Yojson.Safe.to_string payload ~std:true)

let write_as_json_body to_json payload = write_json_body (to_json payload)

let handle_response resp on_success_handler =
  match Cohttp_lwt.Response.status resp with
  | #Cohttp.Code.success_status -> on_success_handler ()
  | s ->
      failwith
        ("Server responded with status "
        ^ Cohttp.Code.(reason_phrase_of_code (code_of_status s)))

let handle_unit_response resp = handle_response resp (fun () -> Lwt.return ())

let read_json_body resp body =
  handle_response resp (fun () ->
      Lwt.(Cohttp_lwt.Body.to_string body >|= Yojson.Safe.from_string))

let read_json_body_as of_json resp body =
  Lwt.(read_json_body resp body >|= of_json)

let read_json_body_as_list resp body =
  Lwt.(read_json_body resp body >|= Yojson.Safe.Util.to_list)

let read_json_body_as_list_of of_json resp body =
  Lwt.(read_json_body_as_list resp body >|= List.map of_json)

let read_json_body_as_map resp body =
  Lwt.(read_json_body resp body >|= Yojson.Safe.Util.to_assoc)

let read_json_body_as_map_of of_json resp body =
  Lwt.(
    read_json_body_as_map resp body >|= List.map (fun (s, v) -> (s, of_json v)))

let replace_string_path_param uri param_name param_value =
  let regexp = Str.regexp (Str.quote ("{" ^ param_name ^ "}")) in
  let path =
    Str.global_replace regexp param_value (Uri.pct_decode (Uri.path uri))
  in
  Uri.with_path uri path

let replace_path_param uri param_name to_string param_value =
  replace_string_path_param uri param_name (to_string param_value)

let maybe_replace_path_param uri param_name to_string param_value =
  option_fold (replace_path_param uri param_name to_string) uri param_value

let add_query_param uri param_name to_string param_value =
  Uri.add_query_param' uri (param_name, to_string param_value)

let add_query_param_list uri param_name to_string param_value =
  Uri.add_query_param uri (param_name, to_string param_value)

let maybe_add_query_param uri param_name to_string param_value =
  option_fold (add_query_param uri param_name to_string) uri param_value

let init_form_encoded_body () = ""

let add_form_encoded_body_param params param_name to_string param_value =
  let new_param_enc =
    Printf.sprintf {|%s=%s|}
      (Uri.pct_encode param_name)
      (Uri.pct_encode (to_string param_value))
  in
  if params = "" then new_param_enc
  else Printf.sprintf {|%s&%s|} params new_param_enc

let add_form_encoded_body_param_list params param_name to_string new_params =
  add_form_encoded_body_param params param_name (String.concat ",")
    (to_string new_params)

let maybe_add_form_encoded_body_param params param_name to_string param_value =
  option_fold
    (add_form_encoded_body_param params param_name to_string)
    params param_value

let finalize_form_encoded_body body = Cohttp_lwt.Body.of_string body

module Multipart_form_data = struct
  let () = Random.self_init ()

  type field = {
    name : string;
    value : string;
    content_type : string option;
    file_name : string option;
  }

  type t = { boundary : string; fields : field list }

  let init_multipart_body () =
    let boundary =
      let chars = "abcdefghijklmnopqrstuvwxyz0123456789" in
      let len = String.length chars in
      let random_char () = chars.[Random.int len] in
      String.init 32 (fun _ -> random_char ())
    in
    { boundary; fields = [] }

  let add_field ~name ~value ?content_type ?file_name t =
    { t with fields = { name; value; content_type; file_name } :: t.fields }

  let make_req ~headers t =
    let headers =
      Cohttp.Header.add headers "Content-Type"
        ("multipart/form-data; boundary=" ^ t.boundary)
    in
    let buf = Buffer.create 1024 in
    let add_field { name; value; content_type; file_name } =
      Buffer.add_string buf ("--" ^ t.boundary ^ "\r\n");
      Buffer.add_string buf
        ("Content-Disposition: form-data; name=\"" ^ name ^ "\"");
      (match file_name with
      | Some fn -> Buffer.add_string buf ("; filename=\"" ^ fn ^ "\"\r\n")
      | None -> Buffer.add_string buf "\r\n");
      (match content_type with
      | Some t -> Buffer.add_string buf ("Content-Type: " ^ t ^ "\r\n")
      | None -> ());
      Buffer.add_string buf "\r\n";
      Buffer.add_string buf value;
      Buffer.add_string buf "\r\n"
    in
    List.iter add_field t.fields;
    Buffer.add_string buf ("--" ^ t.boundary ^ "--\r\n");
    (headers, Cohttp_lwt.Body.of_string (Buffer.contents buf))
end