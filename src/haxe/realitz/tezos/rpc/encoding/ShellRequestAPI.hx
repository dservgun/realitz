package realitz.tezos.rpc.encoding;

/*****************************************************************************/
/*                                                                           */
/* Open Source License                                                       */
/* Copyright (c) Dinkar Ganti, dinkar.ganti@gmail.com                        */
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

import realitz.tezos.rpc.Types;
import haxe.io.Bytes;
import haxe.Http;
import haxe.ds.Option;

class RPCConfig {
  public var url (default, null) : String;
  public var port (default, null) : String;
  public function getHttp() : Http {
    return (new Http("$url:$port"));
  }
}

class Shell {
  static function setLength (aLength : Option<Int>, request : Http) : Http {
    switch (aLength) {
      case None : request;
      case Some(a) : {
        request.setParameter("length", "$a");
      }
    }
    return request;
  }
  static function getChains(config : RPCConfig, chainId : String, 
    length : Option<Int>, 
    head  : Option<BlockHash>, 
    minDate : Option<Date>) : List<List<BlockHash>> {
    var httpRequest : Http =  config.getHttp();
    var httpRequest1 : Http = httpRequest.setParameter("length", httpRequest);

  }
}