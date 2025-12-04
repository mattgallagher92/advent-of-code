[<AutoOpen>]
module Util

open System

let dbg label x =
    printfn $"%s{label}: %A{x}"
    x

[<RequireQualifiedAccess>]
module Int32 =
    let tryParse (s: string) =
        match Int32.TryParse s with
        | true, i -> Some i
        | false, _ -> None

[<RequireQualifiedAccess>]
module Math =
    let integerPower base' exponent =
        if exponent < 0 then
            invalidArg (nameof exponent) $"exponent must be at least 0; given %i{exponent}"
        else
            Seq.replicate exponent base' |> Seq.fold (*) 1L

[<RequireQualifiedAccess>]
module Array =
    let countIf predicate xs =
        xs |> Array.sumBy (fun x -> if predicate x then 1 else 0)

[<RequireQualifiedAccess>]
module Grid =

    type GridDetails<'a> = private {
        Items: 'a array array
        Width: int
        Height: int
    }

    /// A 2D-grid of fixed height and width, indexed by row and column.
    type Grid<'a> =
        private
        | Grid of GridDetails<'a>

        member private this.Details =
            let (Grid gd) = this
            gd

        member this.AllIndexes = [|
            for r in 0 .. this.Details.Height - 1 do
                for c in 0 .. this.Details.Width - 1 do
                    r, c
        |]

        member this.Item(r, c) =
            match this with
            | Grid gd -> gd.Items[r][c]

        member this.ContainsIndex(r, c) =
            r >= 0 && r < this.Details.Height && c >= 0 && c < this.Details.Width

        /// Includes diagonally adjacent.
        member this.IndexesAdjacentTo(r, c) =
            [|
                r - 1, c - 1
                r - 1, c
                r - 1, c + 1

                r + 0, c - 1
                r + 0, c + 1

                r + 1, c - 1
                r + 1, c
                r + 1, c + 1
            |]
            |> Array.filter this.ContainsIndex

        /// Includes diagonally adjacent.
        member this.ItemsAdjacentTo(r, c) =
            this.IndexesAdjacentTo(r, c) |> Array.map this.Item

    /// Returns a grid whose rows are the elements of the given array. Returns an error if the input rows are not all
    // the same length.
    let tryCreate (items: 'a array array) =
        items
        |> Array.map Array.length
        |> Array.distinct
        |> function
            | [||] -> Ok(Grid { Items = items; Width = 0; Height = 0 })
            | [| w |] ->
                Ok(
                    Grid {
                        Items = items
                        Width = w
                        Height = items.Length
                    }
                )
            | widths -> Error $"Grid rows should have the same widths. Distinct row widths given are: %A{widths}."

    /// Returns a grid whose rows are the elements of the given array. Throws if the input rows are not all the same
    /// length.
    let create items =
        items |> tryCreate |> Result.defaultWith failwith

    let indexed (grid: Grid<'a>) =
        let (Grid details) = grid

        details.Items
        |> Array.mapi (fun r row -> row |> Array.mapi (fun c x -> (r, c), x))
        |> Array.collect id
