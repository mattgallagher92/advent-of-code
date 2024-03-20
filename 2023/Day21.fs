module Day21

open System

let parse (lines: string array) =

    let startingPosition =
        lines
        |> Seq.mapi (fun row line -> row, line.IndexOf('S'))
        |> Seq.find (fun (_, col) -> col > -1)

    let map = lines |> array2D

    map, startingPosition

/// Given a map and position, returns the garden plots in the map that are adjacent to that position.
let adjacentGardenPlots map (row, col) =
    seq { row - 1, col; row, col - 1; row, col + 1; row + 1, col }
    |> Seq.filter (fun (r, c) ->
        r > -1
        && r < Array2D.length1 map
        && c > -1
        && c < Array2D.length2 map
        && Array2D.get map r c <> '#')

/// Given a map and a position, returns an array of pairs whose first element is a distance from that position and
/// whose second element is an array containing the positions that are garden plots at that distance from the
/// given position.
let plotsAtDistanceFrom maxDistance map position =

    let rec inner (previousDistance, _ as previousPlots, closerPlots) =

        let plotsWithDistances = (closerPlots, [| previousPlots |]) ||> Array.append

        let nextPlots =

            let plotsAtLowerDistances = plotsWithDistances |> Array.collect snd

            plotsAtLowerDistances
            |> Seq.collect (adjacentGardenPlots map)
            |> Seq.except plotsAtLowerDistances
            |> Seq.toArray

        if nextPlots |> Array.isEmpty || previousDistance = maxDistance then
            plotsWithDistances
        else
            inner (((previousDistance + 1), nextPlots), plotsWithDistances)

    inner ((0, [| position |]), [||])

module PartOne =

    let solve targetSteps lines =

        let map, startingPosition = parse lines

        plotsAtDistanceFrom targetSteps map startingPosition
        // Can double back to get to plots with a lower distance where the difference is even.
        |> Array.filter (fun (distance, _) -> targetSteps - distance >= 0 && (targetSteps - distance) % 2 = 0)
        |> Array.collect snd
        |> Array.length

/// Makes some simplifying assumptions, which apply to the puzzle input I have:
/// - The edge rows and columns contain only garden plots.
/// - The central rows and columns contain only garden plots.
/// - The map is a square whose side length is odd.
/// - The starting position is in the centre of the map.
module PartTwo =

    let parse lines =

        let map, startingPosition = parse lines
        let h, w = map |> Array2D.length1, map |> Array2D.length2
        if h <> w then
            failwith $"Broken assumption: height %i{h} is not equal to width %i{w}."
        elif h % 2 = 0 then
            failwith $"Broken assumption: side length %i{h} is not odd."
        elif startingPosition <> (h / 2, w / 2) then
            failwith $"Broken assumption: starting position %A{startingPosition} is not in the centre of the map."
        else
            let expectedGardenPlots =
                [| 0 .. h - 1 |]
                |> Array.collect (fun row ->
                    if row = 0 || row = h / 2 || row = h - 1 then
                        [| 0 .. h - 1 |]
                    else
                        [|
                            // Left col
                            0

                            // Left diagonal
                            if row < h / 2 then
                                h / 2 - row
                            else
                                row - h / 2

                            // Middle col
                            h / 2

                            // Right diagonal
                            if row < h / 2 then
                                h / 2 + row
                            else
                                3 * (h / 2) - row

                            // Right col
                            h - 1
                        |]
                    |> Array.map (fun col -> row, col)
                )

            match expectedGardenPlots |> Array.tryFind (fun (row, col) -> Array2D.get map row col = '#') with
            | Some (row, col) ->
                failwith $"Broken assumption: (%i{row}, %i{col}) is not a garden plot."
            | None ->
                map, startingPosition

    let solve targetSteps lines =
        printfn "Starting part two"

        let map, _ = parse lines
        /// How many plots are along a single edge of the map.
        let sideLength = map |> Array2D.length1
        /// Shortest distance from the start position to the map edge.
        let distanceToMapEdge = sideLength / 2

        /// A 5 x 5 grid of the original maps, used to calculate counts in various shapes that are repeated in
        /// larger grids.
        let bigMap, bigStartingPosition =

            let gridSize = 5
            let midpoint = 2 * sideLength + distanceToMapEdge

            let repeated (str: string) = String.Join("", Array.replicate gridSize str)
            [| for _ in 1 .. gridSize do yield! lines |> Array.map repeated |]
            |> Array.map (String.map (fun c -> if c = 'S' then '.' else c))
            |> Array.mapi (fun i s ->
                if i = midpoint then
                    s |> String.mapi (fun j c -> if j = midpoint then 'S' else c)
                else
                    s)
            |> parse

        let matchingPositions =
            plotsAtDistanceFrom (2 * sideLength + distanceToMapEdge) bigMap bigStartingPosition
            |> Array.collect (fun (d, positions) -> if (targetSteps - d) % 2 = 0 then positions else [||])

        let countInMap bigCoords =
            matchingPositions
            |> Array.sumBy (fun (row, col) -> if (row / sideLength, col / sideLength) = bigCoords then 1 else 0)

        /// Count in full maps where the matching positions are at an odd distance from the starting position.
        let oddFullMapCount = countInMap (2, 2) |> int64
        /// Count in full maps where the matching positions are at an even distance from the starting position.
        let evenFullMapCount = countInMap (2, 3) |> int64
        /// Count in maps at the corner of the square of positions at the target distance from the starting position.
        let oddCornerMapsCount = [| (0, 2); (2, 0); (2, 4); (4, 2) |] |> Array.sumBy countInMap |> int64
        /// Count in partial maps (along the side of the square) where the matching positions are at an odd distance
        /// from the starting position.
        let oddSideMapsCount = [| (1, 1); (1, 3); (3, 1); (3, 3) |] |> Array.sumBy countInMap |> int64
        /// Count in partial maps (along the side of the square) where the matching positions are at an even
        /// distance from the starting position.
        let evenSideMapCount = [| (1, 0); (1, 4); (3, 0); (3, 4) |] |> Array.sumBy countInMap |> int64

        // The number of maps beyond the central map in one direction.
        let n =
            let x = targetSteps - distanceToMapEdge
            match x % sideLength, x / sideLength with
            | 0, n when n % 2 = 1 -> failwith $"Expected n to be even, but is %i{n}"
            | 0, n -> n |> int64
            | _, _ -> failwith "Expected target distance to be the distance to a map edge."

        (n - 1L) * (n - 1L) * oddFullMapCount
        + n * n * evenFullMapCount
        + oddCornerMapsCount
        + (n - 1L) * oddSideMapsCount
        + n * evenSideMapCount

