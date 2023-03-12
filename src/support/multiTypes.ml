let stringArray_of_yojson x : ([> `StringArray of string array ], string) result
    =
  match x with
  | `List l ->
      let rec aux = function
        | [] -> Ok []
        | `String s :: tl -> Result.map (fun l -> s :: l) (aux tl)
        | _ -> Error "stringArray_of_yojson: not a string array"
      in
      Result.map (fun rl -> `StringArray (Array.of_list rl)) (aux l)
  | _ -> Error "stringArray_of_yojson: not a string array"

let intArray_of_yojson x : ([> `IntArray of int array ], string) result =
  match x with
  | `List l ->
      let rec aux = function
        | [] -> Ok []
        | `Int i :: tl -> Result.map (fun l -> i :: l) (aux tl)
        | _ -> Error "intArray_of_yojson: not an int array"
      in
      Result.map (fun rl -> `IntArray (Array.of_list rl)) (aux l)
  | _ -> Error "intArray_of_yojson: not an int array"

(* TODO: see if there's a less awful way to organize this *)
module StringOrStringArrayOrIntArrayOrIntArrayArray = struct
  type t =
    [ `String of string
    | `StringArray of string array
    | `IntArray of int array
    | `IntArrayArray of int array array ]

  let pp ppf = function
    | `String s -> Format.fprintf ppf "%s" s
    | `StringArray a ->
        Format.fprintf ppf "%a"
          (Format.pp_print_list Format.pp_print_string)
          (Array.to_list a)
    | `IntArray a ->
        Format.fprintf ppf "%a"
          (Format.pp_print_list Format.pp_print_int)
          (Array.to_list a)
    | `IntArrayArray a ->
        Format.fprintf ppf "%a"
          (Format.pp_print_list (Format.pp_print_list Format.pp_print_int))
          (Array.to_list (Array.map Array.to_list a))

  let to_yojson (t : t) : Yojson.Safe.t =
    match t with
    | `String s -> `String s
    | `StringArray a -> `List (List.map (fun s -> `String s) (Array.to_list a))
    | `IntArray a -> `List (List.map (fun i -> `Int i) (Array.to_list a))
    | `IntArrayArray a ->
        `List
          (List.map
             (fun a -> `List (List.map (fun i -> `Int i) (Array.to_list a)))
             (Array.to_list a))

  let of_yojson (x : Yojson.Safe.t) : (t, string) result =
    let errorMessage =
      "StringOrStringArrayOrIntArrayOrIntArrayArray.of_yojson: not a string or \
       string array or int array or int array array"
    in
    match x with
    | `String s -> Ok (`String s)
    | `List l -> (
        (* may still be a string array, int array, or int array array, we can tell by looking at the first element *)
        match l with
        | [] -> Ok (`StringArray [||])
        | `String _ :: _ -> stringArray_of_yojson x
        | `Int _ :: _ -> intArray_of_yojson x
        | `List _ :: _ ->
            (* either an int array array, or invalid *)
            let rec aux = function
              | [] -> Ok []
              | `List l :: tl ->
                  let ( let* ) = Result.bind in
                  let* (`IntArray l') = intArray_of_yojson (`List l) in
                  let* tl' = aux tl in
                  Ok (l' :: tl')
              | _ -> Error errorMessage
            in
            Result.map (fun rl -> `IntArrayArray (Array.of_list rl)) (aux l)
        | _ -> Error errorMessage)
    | _ -> Error errorMessage
end