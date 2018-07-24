package realitz.tezos.rpc;

import haxe.ds.Option;

/*
File : RPC_service.mli

type meth = [ `GET | `POST | `DELETE | `PUT | `PATCH ]

*/
enum Method {
  GET; 
  POST; 
  DELETE;
  PUT; 
  PATCH
}

/*
TODO: Defined in Cohttp.Code. Need to complete this.
*/
enum HttpStatusCode{
  OK;
  Error;
}

/*
type rpc_error =
  | Empty_answer
  | Connection_failed of string
  | Bad_request of string
  | Method_not_allowed of RPC_service.meth list
  | Unsupported_media_type of string option
  | Not_acceptable of { proposed: string ; acceptable: string }
  | Unexpected_status_code of { code: Cohttp.Code.status_code ;
                                content: string ;
                                media_type: string option }
  | Unexpected_content_type of { received: string ;
                                 acceptable: string list ;
                                 body : string }
  | Unexpected_content of { content: string ;
                            media_type: string ;
                            error: string }
  | OCaml_exception of string

*/

enum Error {
  EmptyAnswer;
  ConnectionFailed (message : String);
  BadRequest (message : String);
  MethodNotAllowed (methodList : List<Method>);
  UnsupportedMedia (mediaType : Option<String>); 
  NotAcceptable (proposed : String, acceptable : String);
  UnexpectedStatusCode (code : HttpStatusCode
    , content : String
    , mediaType : Option<String>);
  UnexpectedContentType (received : String, acceptable : List<String>
    , body : String);
  UnexpectedContent (content : String, mediaType : String, error : String);
  OcamlException (exceptionMessage : String);
}
