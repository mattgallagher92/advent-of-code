[<AutoOpen>]
module Util

open System

[<RequireQualifiedAccess>]
module Array =
    /// Finds the indexes of all elements that match the given predicate.
    let findAllIndexes predicate xs =
        xs |> Array.indexed |> Array.filter (snd >> predicate) |> Array.map fst

    let mapSnd f pairs =
        pairs |> Array.map (fun (x, y) -> x, f y)

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

[<RequireQualifiedAccess>]
module Array2D =

    let isWithinBounds array2D (row, col) =
        row > -1
        && row < Array2D.length1 array2D
        && col > -1
        && col < Array2D.length2 array2D

    let row r array2D =
        [| 0 .. (array2D |> Array2D.length2) - 1 |]
        |> Array.map (fun c -> Array2D.get array2D r c)

    let rows array2D =
        [| 0 .. (array2D |> Array2D.length1) - 1 |]
        |> Array.map (fun c -> row c array2D)

    let column c array2D =
        [| 0 .. (array2D |> Array2D.length1) - 1 |]
        |> Array.map (fun r -> Array2D.get array2D r c)

    let columns array2D =
        [| 0 .. (array2D |> Array2D.length2) - 1 |]
        |> Array.map (fun c -> column c array2D)

    // TODO: add tests.
    let tryFindIndex predicate array2D =
        array2D
        |> rows
        |> Seq.map (Array.tryFindIndex predicate)
        |> Seq.indexed
        |> Seq.filter (snd >> Option.isSome)
        |> Seq.map (fun (r, c) -> r, c.Value)
        |> Seq.tryHead

    let findIndex predicate array2D =
        array2D
        |> tryFindIndex predicate
        |> Option.defaultWith (fun _ -> failwith "No matching value.")

    let updateAt r c newVal source =
        let newRow = row r source |> Array.updateAt c newVal
        source |> rows |> Array.updateAt r newRow |> array2D
