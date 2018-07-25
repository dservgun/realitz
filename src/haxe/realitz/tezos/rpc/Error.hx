package realitz.tezos.rpc;
import haxe.ds.Option;
import realitz.tezos.rpc.Types;
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