[<AutoOpen>]
module Util

open System

[<RequireQualifiedAccess>]
module Array =
    /// Finds the indexes of all elements that match the given predicate.
    let findAllIndexes predicate xs =
        xs |> Array.indexed |> Array.filter (snd >> predicate) |> Array.map fst

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
