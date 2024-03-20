module Day11

open System

type Pixel =
    | EmptySpace
    | Galaxy

module Pixel =

    let parse = function '.' -> EmptySpace | '#' -> Galaxy | c -> failwith $"Invalid pixel: %c{c}"

let solve expansionFactor image =

    let universe = image |> Array2D.map Pixel.parse

    let rowNumbers = [| 0 .. (universe |> Array2D.length1) - 1 |]
    let colNumbers = [| 0 .. (universe |> Array2D.length2) - 1 |]

    let galaxyIndices =
        Seq.allPairs rowNumbers colNumbers
        |> Seq.filter (fun (i, j) -> Array2D.get universe i j = Galaxy)
        |> Seq.toArray

    let galaxyPairs =
        galaxyIndices
        |> Seq.mapi (fun i g1 -> galaxyIndices.[ (i+1) .. ] |> Array.map (fun g2 -> g1, g2))
        |> Seq.collect id

    let emptyRowNumbers =
        rowNumbers
        |> Array.filter (fun i -> colNumbers |> Array.forall (fun j -> Array2D.get universe i j = EmptySpace))
        |> Set

    let emptyColNumbers =
        colNumbers
        |> Array.filter (fun j -> rowNumbers |> Array.forall (fun i -> Array2D.get universe i j = EmptySpace))
        |> Set

    let shortestPathLength ((i1: int, j1: int), (i2, j2)) =

        let crossedEmptyRows =
            (if i2 > i1 then Set (seq { i1 .. i2 }) else Set (seq { i2 .. i1 }))
            |> Set.intersect emptyRowNumbers
            |> Set.count
            |> int64

        let crossedEmptyCols =
            (if j2 > j1 then Set (seq { j1 .. j2 }) else Set (seq { j2 .. j1 }))
            |> Set.intersect emptyColNumbers
            |> Set.count
            |> int64

        (Math.Abs(i2 - i1) |> int64)
        + (Math.Abs (j2 - j1) |> int64)
        + (expansionFactor - 1L) * (crossedEmptyRows + crossedEmptyCols)

    galaxyPairs |> Seq.sumBy shortestPathLength

module PartOne =
    let solve = solve 2

module PartTwo =
    let solve = solve 1_000_000L

