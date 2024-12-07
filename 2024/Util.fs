[<AutoOpen>]
module Util

open System

[<RequireQualifiedAccess>]
module Int32 =
    let tryParse (s: string) =
        match Int32.TryParse s with
        | true, i -> Some i
        | false, _ -> None

type Collections.Generic.IDictionary<'a, 'b> with

    member this.TryGet k =
        match this.TryGetValue k with
        | true, v -> Some v
        | false, _ -> None
