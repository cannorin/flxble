(*
The MIT License
DataTypeExtra.fs - Useful wrapper types
Copyright(c) 2018 cannorin
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*)

module DataTypeExtra
open System

/// Function value with equality and comparison based on a fixed randomly-generated guid.
/// Can be used within datatypes without losing structural equality and comparison.
[<CustomEquality; CustomComparison; StructuredFormatDisplay("{AsString}"); Struct>]
type StructuralFunction<'a, 'b> =
  val invoke: 'a -> 'b
  val guid: Guid
  val private str: string;
  
  new (f, guid) =
    { invoke = f; guid = guid;
      str = sprintf "structural[%s]:%A" (guid.ToString()) f }

  /// Creates an instance of `StructuralFunction`.
  new (f) =
    let hashcode = Guid.NewGuid()
    StructuralFunction(f, hashcode)
  
  member this.AsString = this.str
  member inline this.Invoke x = this.invoke x
  override this.ToString() = this.AsString
  override x.Equals yo =
    match yo with
      | :? StructuralFunction<'a, 'b> as y -> Guid.(=) (x.guid, y.guid)
      | _ -> false 
  override this.GetHashCode() = this.guid.GetHashCode()
  interface IComparable with
    member x.CompareTo yo =
      match yo with
        | :? StructuralFunction<'a, 'b> as y -> compare x.guid y.guid
        | _ -> invalidArg "yo" "cannot compare values of different types"

/// Value of which equality completely ignored.
/// Any value of this type will be treated as equal.
[<CustomEquality; CustomComparison; StructuredFormatDisplay("{AsString}"); Struct>]
type EqualityNull<'T> =
  | EValue of 'T
  | ENull
  with
  member inline this.AsString =
    match this with
      | EValue value -> sprintf "EqualityNull (%A)" value
      | ENull        -> sprintf "EqualityNull"
  override x.Equals(yobj) =
    match yobj with
      | :? EqualityNull<'T> -> true
      | _ -> false
  override x.GetHashCode() = 0
  interface IComparable with
    member x.CompareTo yo =
      match yo with
        | :? EqualityNull<'T> -> 0
        | _ -> invalidArg "yo" "cannot compare values of different types"

/// Creates an instance of `EqualityNull<_>`.
let inline EqualityNull x : EqualityNull<_> = EValue x

module EqualityNull =
  let empty   = ENull
  let inline bind f x = match x with EValue v -> f v | ENull -> ENull
  let inline map f x = match x with EValue v -> EValue (f v) | ENull -> ENull
  let inline defaultValue dv x = match x with EValue v -> v | ENull -> dv
  let inline defaultWith df x = match x with EValue v -> v | ENull -> df ()
  let inline toOption x = match x with EValue v -> Some v | ENull -> None
  let inline ofOption o = match o with Some x -> EValue x | None -> ENull

type EqualityNull<'T> with
  static member inline (>>=) (x, f) = EqualityNull.bind f x
  static member inline Return x = EValue x
  static member inline Map (x, f) = EqualityNull.map f x
  static member inline Zero = ENull

/// Wrapper type to help creating a discremated union with
/// a shared property (`info`) among the cases.
[<Struct>]
type With<'Info, 'Item> =
  { item: 'Item; info: 'Info option }
  override x.ToString() = to_s x.item

module With =
  let inline itemOf x = x.item
  let inline infoOf x = x.info
  let inline bind f x : With<_, _> = f x.item
  let inline map f x = { item = f x.item; info = x.info }
  let inline mapInfo f x = { item = x.item; info = Option.map f x.info }
  let inline bimap f g x = { item = f x.item; info = Option.map g x.info }
  let inline sameInfoOf orig x = { item = x; info = orig.info }
  let inline info i x = { item = x; info = Some i }
  let inline noInfo x = { item = x; info = None }

type With<'T, 'U> with
  static member inline Bimap (x, f, g) = With.bimap f g x
  static member inline Map (x, f) = With.map f x
  static member inline (>>=) (x, f) = With.bind f x
  static member inline Return x = With.noInfo x
 
let (|With|) x = With (x.item, x.info)
let (|Item|) x = Item x.item
let (|Info|) x = Info x.info