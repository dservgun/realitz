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

import realitz.tezos.rpc.Types;
import haxe.io.Bytes;
/*
  $michelson.v1.primitives:
    | "lambda" | "operation" | "EMPTY_SET"
    | "SWAP" | "MEM" | "RIGHT"
    | "CONTRACT" | "or"
    | "CONCAT" | "nat" | "bytes"
    | "Unit" | "Some"
    | "UNPACK" | "NOT"
    | "LEFT" | "timestamp"
    | "AMOUNT" | "DROP"
    | "ABS" | "contract"
    | "GE" | "PUSH"
    | "LT" | "address"
    | "NEQ" | "NEG"
    | "None" | "CONS"
    | "EXEC" | "NIL"
    | "CAST" | "MUL"
    | "ADDRESS" | "EDIV"
    | "STEPS_TO_QUOTA" | "SUB"
    | "INT" | "SOURCE"
    | "CAR" | "CREATE_ACCOUNT"
    | "LSL" | "OR" | "IF_NONE"
    | "SELF" | "IF"
    | "Left" | "int"
    | "big_map" | "SENDER"
    | "option" | "DUP"
    | "EQ" | "NOW" | "key_hash"
    | "GET" | "list"
    | "key" | "True"
    | "GT" | "parameter"
    | "IF_LEFT" | "FAILWITH"
    | "PAIR" | "LOOP_LEFT"
    | "Pair" | "RENAME"
    | "EMPTY_MAP" | "CREATE_CONTRACT" | "HASH_KEY"
    | "ISNAT" | "code" | "AND"

*/
//M prefix for dealing with potentially ambiguous labels.
enum MichelsonV1Primitive {
  ADD (disp : String); LE (disp : String); UPDATE (disp : String);
  Unit (disp : String); MString (disp : String); 
  MCompare (disp : String); MLambda (disp : String);
  LOOP (disp : String); ELT (disp : String); ImplicitAccount (disp : String);
  NONE (disp : String); Signature (disp : String); SetVal (disp : String);
  Mutez (disp : String); Blake2B (disp : String); SHA256 (disp : String);
  ITER (disp : String); MBool (disp : String); MMap (disp : String);
  IFCons (disp : String); LSR (disp : String); SetDelegate (disp : String);
  Storage (disp : String); XOR (disp : String); CDR (disp : String); 
  TransferTokens (disp : String); MSome (disp : String); MFalse (disp : String);
  SHA512 (disp : String); CheckSignature (disp : String); 
  Balance (disp : String);
  IsNat (disp : String) ; MCode (disp : String); MAND (disp : String); 
  EmptyMap (disp : String); CreateContract (disp : String); 
  HashKey (disp : String); Pair (disp : String);
}
enum MichelsonV1Expression {
  IntExpression(exp : BigNum);
  StringExpression(exp : String);
  ByteExpression (exp : Bytes);
  MichelsonV1Expressions(exps : List<MichelsonV1Expression>);
  PrimitiveExpression(prim : MichelsonV1Primitive, args : MichelsonV1Expression, annotations : List<String>); 
}

/*
 $micheline.michelson_v1.expression:
    { "int": $bignum }
    || { "string": string }
    || { "bytes": /^[a-zA-Z0-9]+$/ }
    || [ $micheline.michelson_v1.expression ... ]
    || { "prim": $michelson.v1.primitives,
         "args"?: [ $micheline.michelson_v1.expression ... ],
         "annots"?: [ string ... ] }
 
*/
