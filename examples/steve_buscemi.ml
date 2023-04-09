let convert_image img =
  let () = Printf.printf "About to identify image: %s\n" img in
  let ic =
    Unix.open_process_args_in "identify"
      [| "-ping"; "-format"; "%w %h\\n"; img |]
  in
  let () = print_endline "Opened process" in
  let s = input_line ic in
  let () = print_endline "Read line process" in
  let () = close_in ic in
  let () = Printf.printf "Done identifying image\n" in
  let w, h = Scanf.sscanf s "%d %d" (fun w h -> (w, h)) in
  let () = Printf.printf "w: %d, h: %d\n" w h in
  (* let dim = if w > h then w else h in *)
  (* let tmp = Filename.temp_file "img" ".png" in *)
  let tmp = "/tmp/img0f4080.png" in
  let () = Printf.printf "tmp : %s\n" tmp in
  (* run imagemagick in the shell  *)
  let code =
    (* let args = [| img; tmp |] in
       let () = print_endline ([%show: string array] args) in
       Unix.open_process_args_in "convert" args *)
    Sys.command (Printf.sprintf "convert %s -resize 1500x1500! %s" img tmp)
  in
  (* wait for EOF from ic *)
  let () = Printf.printf "code: %d\n" code in
  tmp

let () =
  let image = convert_image Sys.argv.(1) in
  Lwt_main.run
    (let _req = Openai.Create_image_variation_request.make ~image in
     let%lwt image = Lwt_io.(with_file ~mode:input image read) in
     (* write the string image to a file called "testOutput.png" *)
     let%lwt () =
       Lwt_io.(with_file ~mode:output "testOutput.png" (fun f -> write f image))
     in
     let%lwt res = Openai.Open_ai_api.create_image_variation ~image in
     let%lwt () =
       Lwt_io.printl
         ([%show: Openai.Images_response_data.t list]
            res.Openai.Images_response.data)
     in
     Lwt.return_unit)
