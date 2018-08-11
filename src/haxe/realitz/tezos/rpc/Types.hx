/*****************************************************************************/
/*                                                                           */
/* Open Source License                                                       */
/* Copyright (c) Dinkar Ganti, dinkar.ganti@gmail.com */
/*                                                                           */
/* Permission is hereby granted, free of charge, to any person obtaining a   */
/* copy of this software and associated documentation files (the "Software"),*/
/* to deal in the Software without restriction, including without limitation */
/* the rights to use, copy, modify, merge, publish, distribute, sublicense,  */
/* and/or sell copies of the Software, and to permit persons to whom the     */
/* Software is furnished to do so, subject to the following conditions:      */
/*                                                                           */
/* The above copyright notice and this permission notice shall be included   */
/* in all copies or substantial portions of the Software.                    */
/*                                                                           */
/* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*/
/* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  */
/* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   */
/* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*/
/* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   */
/* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       */
/* DEALINGS IN THE SOFTWARE.                                                 */
/*                                                                           */
/*****************************************************************************/

package realitz.tezos.rpc;

import haxe.ds.Option;
import haxe.Int64;
import haxe.io.Bytes;
import realitz.tezos.rpc.BlockHeader;
import realitz.tezos.rpc.ConnectionInfo;
import realitz.tezos.rpc.Peer;

typedef Protocol = String;
typedef BigNum = String;
typedef Mutez = String;
typedef PositiveBigNum = String;

class ProtocolHash {  
  public var hash (default, null) : String; 
  public function new(aString : String) {
    hash = aString;
  }
}

class BlockHeader {
  var level :  Int;
  var protoLevel : Int ;
  var predecessor : Bytes ;
  var timestamp : Bytes;
  var validationPass : Int; 
  var operationsHash : Array<Array<Bytes>>;
  var fitness : Array<Bytes>; 
  var context : Bytes;
}

enum Nonces {
  Nonce (nonce : Bytes);
  Hash (hash : String);
}

class ContextHash {
  var hash : String;
  public function new (jsonString : String) {
    hash = haxe.Json.parse(jsonString);
  }
}
class OperationHash {
  var hash : String;
}

class InlinedEndorsement {
  var branch : BlockHash;
  var operations : InlinedEndorsementContent;
  var signature : Signature;
}


/*
File : RPC_service.mli

type meth = [ `GET | `POST | `DELETE | `PUT | `PATCH ]

*/
enum Method {
  GET; 
  POST; 
  DELETE;
  PUT; 
  PATCH;
}

/*
TODO: Defined in Cohttp.Code. Need to complete this.
*/
enum HttpStatusCode{
  OK;
  Error;
}

typedef Time = Int64;
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


//TODO: Refactor this into some other class/enum.
enum RPCServiceError {
  Exception(message : String);
  Canceled;
}
/*
file: RPC_answer.mli
(** Return type for service handler *)
type 'o t =
  [ `Ok of 'o (* 200 *)
  | `OkStream of 'o stream (* 200 *)
  | `Created of string option (* 201 *)
  | `No_content (* 204 *)
  | `Unauthorized of RPC_service.error option (* 401 *)
  | `Forbidden of RPC_service.error option (* 403 *)
  | `Not_found of RPC_service.error option (* 404 *)
  | `Conflict of RPC_service.error option (* 409 *)
  | `Error of RPC_service.error option (* 500 *)
  ]
*/

enum Answer {
  Ok(code : Int);
  Created(code : Option<String>);
  NoContent (code : Int);
  Unauthorized(errorCode : Option<RPCServiceError>);
  Forbidden (errorCode : Option<RPCServiceError>);
  NotFound (errorCode : Option<RPCServiceError>);
  Conflict (errorCode : Option<RPCServiceError>);
  Error(error : String);
}

class BlockHash {
  public var hash (default, null) : String;
  public function new (aString : String) {
    hash = aString;
  }
}

class ChainId {
  public var hash (default, null) : String;
  public function new (aString : String) {
    hash = aString;
  }
}

typedef PeerId = PublicKey
typedef ConnectionId = PublicKey
typedef Baker = PublicKey
typedef PublicKeyHash = BlockHash
typedef OperationsHash = BlockHash
class PublicKey {
  public var hash (default, null) : String;
}

class IdPoint {
  var address : String;
  var port : Int;
  public function new (aDyn : Dynamic) {
    trace('IdPoint $aDyn');
    address = aDyn.addr;
    port = aDyn.port;
  }
  public static function parseJSON(aDyn : Dynamic) {
    return (new IdPoint(aDyn));
  }
}

class ConnectionVersion {
  var name : String;
  var major : Int;
  var minor : Int;
  public function new (dyn : Dynamic) {
    name = dyn.name;
    major = dyn.major;
    minor = dyn.minor;
  }
  public static function fromArray(dynList : Array<Dynamic>) : List<ConnectionVersion> {
    var result : List<ConnectionVersion> = new List();
    for (aDyn in dynList) {
      trace(aDyn);
      result.add(new ConnectionVersion(aDyn));
    }
    return result;
  }
}

class ConnectionInformation {
  var incoming : Bool;
  public var peerId (default, null) : PeerId;
  var idPoint : IdPoint;
  var remoteSocketPort : Int;
  var versions : List<ConnectionVersion>;
  public function new (aDyn : Dynamic) {
    trace("Creating connection information"); 
    incoming = aDyn.incoming;
    peerId = aDyn.peer_id;
    idPoint = new IdPoint(aDyn.id_point);
    remoteSocketPort = aDyn.remote_socket_port;
    versions = ConnectionVersion.fromArray(aDyn.versions);
  }

  public static function parseJSON(dyn : Array<Dynamic>) : List<ConnectionInformation> {
    var result : List<ConnectionInformation> = new List();
    try {
      for (aDyn in dyn) {
        var connInfo = new ConnectionInformation(aDyn);
        trace('Adding $connInfo');
        result.add(connInfo);
      }
      return result;      
    }catch(err : String) {
      trace('$err');
      return (new List());
    }
  }
}

/*
  $inlined.endorsement:
    { "branch": $block_hash,
      "operations": $inlined.endorsement.contents,
      "signature"?: $Signature }
  $inlined.endorsement.contents:
    { "kind": "endorsement",
      "level": integer ∈ [-2^31-2, 2^31+2] }
*/
class Signature {
  var signature : String;
  var publicKey : PublicKey;
  var publicKeyHash : PublicKeyHash;
}

/*
  $inlined.endorsement.contents:
    { "kind": "endorsement",
      "level": integer ∈ [-2^31-2, 2^31+2] }
*/

enum EndorsementKind {
  Endorsement;
}
class InlinedEndorsementContent {
  var kind : EndorsementKind;
  var level : Int;
}

class PeerPair{
  public var peerId (default, null): PeerId;
  public var peer (default, null) : Peer;
  public function new(pId : PeerId, p : Peer) {
    peerId = pId;
    peer = p;
  }
}

class PointPair {
  public var peerId (default, null) : PeerId;
  public var point (default, null ): Point;
  public function new (pId : PeerId, p : Point) {
    peerId = pId;
    point = p;
  }
}

typedef ContractId = BlockHash
enum ContractKind {
  Freezer;
}

class EndorsementRight {
  var level (default, null) : Int;
  var delegate (default, null) : PublicKeyHash;
  var slots : List<Int>; 
  var estimatedTime : Date;
}

class BlockResponse {
  var block : BlockHash; 
  var level : Int ;
  var errors : List<Error>;
}
