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


//TODO: Is the name right? Will Injection<type> read better than Inject<Type>
// The reason we chose Inject is because that is how the current spec is defining
// these operations.

class InjectBlock {
  var data : Bytes;
  var operations : 
    List <List < {branch : BlockHash, data : Bytes} > >;
  private static function getBlockOperations(opsList : List<List< {branch : BlockHash, data : Bytes}>>) : List< List <{branch : String, data : Bytes}>> {
    var result : List<List<{branch : String, data : Bytes}>> = new List();
    for (innerList in opsList) {
      var elementList : List<{branch : String, data : Bytes}> = 
        new List();
      for (element in innerList) {
        var nElement = {branch :element.branch.hash, data : element.data};
        elementList.add(nElement);
      }
      result.add(elementList);
    }
    return result;
  }
  public static function toDynamic(aBlock : InjectBlock) : Dynamic {
    var result : Dynamic = {
      data : aBlock.data, 
      operations : getBlockOperations(aBlock.operations)
    };
    return result;
  }
}

class InjectOperation {
  public var data (default, null) : String;
}


class Component {
  var name : String ;
  var _interface : Bytes;
  var implementation : Bytes;
  function new(aName : String, anInterface : Bytes, anImplementation : Bytes) {
    name = aName;
    _interface = anInterface;
    implementation = anImplementation;
  }
  public static function toJSON (components : List<Component>) : 
    List<Dynamic> {
    var result : List<Dynamic> = new List();
    for (aComponent in components) {
      result.add({"name" : aComponent.name, "interface" : aComponent._interface, 
        "implementation" : aComponent.implementation
        });
    }
    return result;
  }
  public static function fromJSON(components : Array<Dynamic>) : List<Component> {
    var result : List<Component> = new List();
    for (component in components){
      var name = component.name;
      var interface_ = component.get("interface");
      var implementation = component.implementation;
      result.add(new Component(name, interface_, implementation));
    }
    return result;
  }
}


class InjectProtocol {
  var expectedEnvVersion : Int;
  var components : List<Component>;
  
  public function new (envVersion : Int, componentsList : List<Component>) {
    expectedEnvVersion = envVersion;
    components = componentsList;
  }
  public function toJSON() : String {
    var dyn : Dynamic = 
      {
        "expected_env_version" : expectedEnvVersion,
        "components" : Component.toJSON(components)
      };
    return haxe.Json.stringify(dyn);
  }

  public static function parseJSON(dyn : Dynamic) : InjectProtocol {
    var envVersion = dyn.expected_env_version;
    var components : List<Component> = Component.fromJSON(dyn.components);
    return (new InjectProtocol(envVersion, components));
  }

} 

