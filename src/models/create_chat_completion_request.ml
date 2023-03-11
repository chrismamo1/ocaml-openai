(*
 * This file has been generated by the OCamlClientCodegen generator for openapi-generator.
 *
 * Generated by: https://openapi-generator.tech
 *
 *)

type t = {
    (* ID of the model to use. Currently, only `gpt-3.5-turbo` and `gpt-3.5-turbo-0301` are supported. *)
    model: string;
    (* The messages to generate chat completions for, in the [chat format](/docs/guides/chat/introduction). *)
    messages: Chat_completion_request_message.t list;
    (* completions_temperature_description *)
    temperature: float option [@default None];
    (* completions_top_p_description *)
    top_p: float option [@default None];
    (* How many chat completion choices to generate for each input message. *)
    n: int32 option [@default None];
    (* If set, partial message deltas will be sent, like in ChatGPT. Tokens will be sent as data-only [server-sent events](https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#Event_stream_format) as they become available, with the stream terminated by a `data: [DONE]` message.  *)
    stream: bool option [@default None];
    (* Up to 4 sequences where the API will stop generating further tokens.  *)
    stop: string array option [@default None];
    (* The maximum number of tokens allowed for the generated answer. By default, the number of tokens the model can return will be (4096 - prompt tokens).  *)
    max_tokens: int32 option [@default None];
    (* completions_presence_penalty_description *)
    presence_penalty: float option [@default None];
    (* completions_frequency_penalty_description *)
    frequency_penalty: float option [@default None];
    (* Modify the likelihood of specified tokens appearing in the completion.  Accepts a json object that maps tokens (specified by their token ID in the tokenizer) to an associated bias value from -100 to 100. Mathematically, the bias is added to the logits generated by the model prior to sampling. The exact effect will vary per model, but values between -1 and 1 should decrease or increase likelihood of selection; values like -100 or 100 should result in a ban or exclusive selection of the relevant token.  *)
    logit_bias: Yojson.Safe.t option [@default None];
} [@@deriving yojson { strict = false }, show ];;

let create (model : string) (messages : Chat_completion_request_message.t list) : t = {
    model = model;
    messages = messages;
    temperature = None;
    top_p = None;
    n = None;
    stream = None;
    stop = None;
    max_tokens = None;
    presence_penalty = None;
    frequency_penalty = None;
    logit_bias = None;
}

