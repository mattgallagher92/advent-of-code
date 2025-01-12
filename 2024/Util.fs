[<AutoOpen>]
module Util

open System

module Regex =
    open System.Text.RegularExpressions

    let regexMatch pattern input = Regex.Match(input, pattern)

[<RequireQualifiedAccess>]
module Array =
    /// Finds the indexes of all elements that match the given predicate.
    let findAllIndexes predicate xs =
        xs |> Array.indexed |> Array.filter (snd >> predicate) |> Array.map fst

    let mapSnd f pairs =
        pairs |> Array.map (fun (x, y) -> x, f y)

    let withIndexMatching predicate xs =
        xs |> Array.indexed |> Array.filter (fst >> predicate) |> Array.map snd

    let rec private findFirstInternal bad good predicate (xs: 'a array) =
        if bad + 1 = good then
            good
        else
            let test = (bad + good) / 2

            if predicate xs[test] then
                findFirstInternal bad test predicate xs
            else
                findFirstInternal test good predicate xs

    /// Finds the first element matching predicate, assuming that they all don't match up to a point, then all match.
    // TODO: add property-based test to compare against findIndex.
    let findFirstIndexQuickly predicate xs =
        let n = xs |> Array.length
        findFirstInternal 0 (n - 1) predicate xs

[<RequireQualifiedAccess>]
module Int32 =
    let tryParse (s: string) =
        match Int32.TryParse s with
        | true, i -> Some i
        | false, _ -> None

[<RequireQualifiedAccess>]
module String =
    let removeAt j (s: string) =
        s |> Seq.toArray |> Array.removeAt j |> (fun cs -> String cs |> string)

type Collections.Generic.IDictionary<'a, 'b> with

    member this.TryGet k =
        match this.TryGetValue k with
        | true, v -> Some v
        | false, _ -> None

[<RequireQualifiedAccess>]
module Pair =

    let asArray (x, y) = [| x; y |]

    let mapFst f (x, y) = f x, y

    let add (x2, y2) (x1, y1) = x1 + x2, y1 + y2

    let subtract (x0, y0) (x1, y1) = x1 - x0, y1 - y0

    let ortholinearDistance (x0: int, y0: int) (x1, y1) = Math.Abs(x1 - x0) + Math.Abs(y1 - y0)

[<RequireQualifiedAccess>]
module Option =
    let noneIfFalse predicate x = if predicate x then Some x else None

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

    let indexed array2D =
        array2D
        |> rows
        |> Array.indexed
        |> Array.collect (fun (r, vs) -> vs |> Array.mapi (fun c v -> (r, c), v))

    let indexes array2D = [|
        for r in 0 .. Array2D.length1 array2D - 1 do
            for c in 0 .. Array2D.length2 array2D - 1 do
                r, c
    |]


    let tryGet array2D row col =
        (row, col)
        |> Option.noneIfFalse (isWithinBounds array2D)
        |> Option.map (fun coords -> coords ||> Array2D.get array2D)

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

    let findAllIndexes predicate array2D =
        array2D
        |> rows
        |> Array.indexed
        |> Array.collect (fun (r, vs) -> vs |> Array.findAllIndexes predicate |> Array.map (fun c -> r, c))

    let adjacentIndexes array2D (r, c) =
        [| r - 1, c; r, c - 1; r, c + 1; r + 1, c |]
        |> Array.filter (isWithinBounds array2D)

    let updateAt r c newVal source =
        let newRow = row r source |> Array.updateAt c newVal
        source |> rows |> Array.updateAt r newRow |> array2D
