class read_line ~term ~history ~state =
  object (self)
    inherit LTerm_read_line.read_line ~history ()
    inherit [Zed_string.t] LTerm_read_line.term term
    method! show_box = false

    initializer
    self#set_prompt
      (React.S.const (LTerm_text.of_string @@ Zed_string.of_utf8 "\\>"))
  end

let rec loop term history state =
  let%lwt input =
    Lwt.catch
      (fun () ->
        let rl =
          new read_line ~term ~history:(LTerm_history.contents history) ~state
        in
        let%lwt command = rl#run in
        Lwt.return_some command)
      (function Sys.Break -> Lwt.return None | exn -> Lwt.fail exn)
  in
  match input with
  | Some input ->
      let message =
        Openai.Chat_completion_request_message.make ~role:`User
          ~content:(Zed_string.to_utf8 input) ()
      in
      let state = state @ [ message ] in
      let req =
        Openai.Create_chat_completion_request.make ~model:"gpt-3.5-turbo"
          ~messages:state ()
      in
      (* n = 1 so we can assume the list is always shaped like this *)
      let%lwt[@warning "-8"] { choices = [ { message = message'; _ } ]; _ } =
        Openai.Open_ai_api.create_chat_completion
          ~create_chat_completion_request_t:
            { req with messages = state; n = Some 1l }
      in
      let state =
        state
        @ [ { role = message'.role; content = message'.content; name = None } ]
      in
      let label = new LTerm_widget.label message'.content in
      let () = label#set_alignment LTerm_geom.H_align_right in
      let%lwt () =
        let term_width = LTerm.size term |> LTerm_geom.cols in
        let text_width = if term_width > 80 then 80 else term_width in
        let message'' =
          let lines = String.split_on_char '\n' message'.content in
          let rec aux = function
            | [] -> []
            | line :: lines ->
                if String.length line > text_width then
                  let line' = String.sub line 0 text_width in
                  let line'' =
                    String.sub line text_width (String.length line - text_width)
                  in
                  line' :: aux (line'' :: lines)
                else line :: aux lines
          in
          aux lines |> String.concat "\n"
        in
        LTerm_text.(
          eval
            [
              B_bold true;
              B_fg LTerm_style.black;
              B_bg (LTerm_style.rgb 128 255 128);
              S message'';
              E_bg;
              E_fg;
              E_bold;
            ])
        |> LTerm.fprintls term
      in
      LTerm_history.add history input;
      loop term history state
  | None -> loop term history state

let chatgpt initial_state =
  let%lwt () = LTerm_inputrc.load () in
  Lwt.catch
    (fun () ->
      let state = initial_state in
      let%lwt term = Lazy.force LTerm.stdout in
      loop term (LTerm_history.create []) state)
    (function
      | LTerm_read_line.Interrupt -> Lwt.return () | exn -> Lwt.fail exn)

let _ =
  let system_prompt =
    Openai.Chat_completion_request_message.make ~role:`System
      ~content:
        "You are a general-purpose assistant who provides detailed, helpful \
         responses."
      ()
  in
  let init_message =
    Openai.Chat_completion_request_message.make ~role:`User
      ~content:"Hello! What are you?" ()
  in
  Lwt_main.run (chatgpt [ system_prompt; init_message ])
