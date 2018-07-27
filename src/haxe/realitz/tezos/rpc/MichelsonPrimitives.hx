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
*/
enum MichelsonV1Primitive {
  ABS (disp : String); 
  ADD (disp : String); 
  Address (disp : String); 
  Amount (disp : String); 
  Balance (disp : String);
  BigMap (disp : String); 
  Blake2B (disp : String); 
  Bytes (disp : String);
  CAR (disp : String); 
  Cast (disp : String); 
  CDR (disp : String); 
  CheckSignature (disp : String); 
  Concat (disp : String); 
  Cons (disp : String); 
  Contract (disp : String);
  CONTRACT (disp : String); 
  CreateAccount (disp : String);
  CreateContract (disp : String); 
  Drop (disp : String); 
  DUP (disp : String); 
  EDiv (disp : String);
  ELT (disp : String); 
  EmptyMap (disp : String); 
  EmptySet (disp : String);
  EQ (disp : String); 
  Exec (disp : String); 
  FailWith (disp : String);
  GE (disp : String); 
  GT (disp : String); 
  HashKey (disp : String); 
  IFCons (disp : String); 
  IFLeft (disp : String); 
  IFNone (disp : String);
  ImplicitAccount (disp : String);
  IsNat (disp : String); 
  ITER (disp : String); 
  Key (disp : String); 
  KeyHash (disp : String); 
  Lambda (disp : String); 
  LE (disp : String); 
  Left (disp : String);
  LEFT (disp : String); 
  LOOP (disp : String); 
  LoopLeft (disp : String);
  LSL (disp : String);
  LSR (disp : String); 
  LT (disp : String); 
  MAND (disp : String); 
  MBool (disp : String); 
  MCode (disp : String); 
  MCompare (disp : String); 
  Mem (disp : String); 
  MFalse (disp : String);
  MGet (disp : String); 
  MIf (disp : String);
  MInt (disp : String);
  MLambda (disp : String);
  MList (disp : String);
  MMap (disp : String);
  MOption (disp : String); 
  MOR (disp : String);
  MOr (disp : String); 
  MSome (disp : String); 
  MString (disp : String); 
  Mul (disp : String);
  Mutez (disp : String); 
  Nat (disp : String); 
  NEG (disp : String);
  NEQ (disp : String); 
  Nil (disp : String); 
  NONE (disp : String); 
  None (disp : String); 
  Now (disp : String); 
  Operation (disp : String); 
  Pair (disp : String); 
  Push (disp : String);
  Rename (disp : String); 
  Self (disp : String);
  Sender (disp : String);
  SetDelegate (disp : String);
  SetVal (disp : String);
  SHA256 (disp : String);
  SHA512 (disp : String); 
  Signature (disp : String); 
  Source (disp : String); 
  StepsToQuota (disp : String);
  Storage (disp : String); 
  Sub (disp : String); 
  Swap (disp : String); 
  Timestamp (disp : String); 
  TransferTokens (disp : String); 
  Unit (disp : String); 
  UPDATE (disp : String);
  XOR (disp : String); 
}
