(*
 * This file has been generated by the OCamlClientCodegen generator for openapi-generator.
 *
 * Generated by: https://openapi-generator.tech
 *
 *)

type t = {
    (* The audio file to translate, in one of these formats: mp3, mp4, mpeg, mpga, m4a, wav, or webm.  *)
    file: string;
    (* ID of the model to use. Only `whisper-1` is currently available.  *)
    model: string;
    (* An optional text to guide the model's style or continue a previous audio segment. The [prompt](/docs/guides/speech-to-text/prompting) should be in English.  *)
    prompt: string option [@default None];
    (* The format of the transcript output, in one of these options: json, text, srt, verbose_json, or vtt.  *)
    response_format: string option [@default None];
    (* The sampling temperature, between 0 and 1. Higher values like 0.8 will make the output more random, while lower values like 0.2 will make it more focused and deterministic. If set to 0, the model will use [log probability](https://en.wikipedia.org/wiki/Log_probability) to automatically increase the temperature until certain thresholds are hit.  *)
    temperature: float option [@default None];
} [@@deriving yojson { strict = false }, show, make ];;
