(*
 * This file has been generated by the OCamlClientCodegen generator for openapi-generator.
 *
 * Generated by: https://openapi-generator.tech
 *
 *)

val cancel_fine_tune : fine_tune_id:string -> Fine_tune.t Lwt.t

val create_answer :
     create_answer_request_t:Create_answer_request.t
  -> Create_answer_response.t Lwt.t

val create_chat_completion :
     create_chat_completion_request_t:Create_chat_completion_request.t
  -> Create_chat_completion_response.t Lwt.t

val create_classification :
     create_classification_request_t:Create_classification_request.t
  -> Create_classification_response.t Lwt.t

val create_completion :
     create_completion_request_t:Create_completion_request.t
  -> Create_completion_response.t Lwt.t

val create_edit :
  create_edit_request_t:Create_edit_request.t -> Create_edit_response.t Lwt.t

val create_embedding :
     create_embedding_request_t:Create_embedding_request.t
  -> Create_embedding_response.t Lwt.t

val create_file : file:string -> purpose:string -> Open_ai_file.t Lwt.t

val create_fine_tune :
  create_fine_tune_request_t:Create_fine_tune_request.t -> Fine_tune.t Lwt.t

val create_image :
  create_image_request_t:Create_image_request.t -> Images_response.t Lwt.t

val create_image_edit :
     image:string
  -> prompt:string
  -> ?mask:string
  -> unit
  -> Images_response.t Lwt.t

val create_image_variation : image:string -> Images_response.t Lwt.t

val create_moderation :
     create_moderation_request_t:Create_moderation_request.t
  -> Create_moderation_response.t Lwt.t

val create_search :
     engine_id:string
  -> create_search_request_t:Create_search_request.t
  -> Create_search_response.t Lwt.t

val create_transcription :
     file:string
  -> model:string
  -> ?prompt:string
  -> ?response_format:string
  -> ?temperature:float
  -> ?language:string
  -> unit
  -> Create_transcription_response.t Lwt.t

val create_translation :
     file:string
  -> model:string
  -> ?prompt:string
  -> ?response_format:string
  -> ?temperature:float
  -> unit
  -> Create_translation_response.t Lwt.t

val delete_file : file_id:string -> Delete_file_response.t Lwt.t

val delete_model : model:string -> Delete_model_response.t Lwt.t

val download_file : file_id:string -> string Lwt.t

val list_engines : unit -> List_engines_response.t Lwt.t

val list_files : unit -> List_files_response.t Lwt.t

val list_fine_tune_events :
     fine_tune_id:string
  -> ?stream:bool
  -> unit
  -> List_fine_tune_events_response.t Lwt.t

val list_fine_tunes : unit -> List_fine_tunes_response.t Lwt.t

val list_models : unit -> List_models_response.t Lwt.t

val retrieve_engine : engine_id:string -> Engine.t Lwt.t

val retrieve_file : file_id:string -> Open_ai_file.t Lwt.t

val retrieve_fine_tune : fine_tune_id:string -> Fine_tune.t Lwt.t

val retrieve_model : model:string -> Model.t Lwt.t
