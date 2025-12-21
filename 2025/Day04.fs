module Day04

let parseLines lines =
    lines |> Array.map Seq.toArray |> Grid.create

module PartOne =

    let solve (lines: string array) =
        let grid = parseLines lines

        grid
        |> Grid.indexed
        |> Array.countIf (fun (ix, x) ->
            let adjacentRollsCount = ix |> grid.ItemsAdjacentTo |> Array.countIf ((=) '@')
            x = '@' && adjacentRollsCount < 4)

module PartTwo =

    open System.Collections.Generic

    /// Returns a grid where the value is Some countOfAdjacentRolls when there is a roll present at that location, None otherwise.
    let parseLines (lines: string array) =
        let grid = parseLines lines

        grid
        |> Grid.mapi (fun r c ->
            function
            | '@' -> grid.ItemsAdjacentTo(r, c) |> Array.countIf ((=) '@') |> Some
            | '.' -> None
            | ch -> failwith $"Invalid char in input: %c{ch}")

    let solve (lines: string array) =
        let grid = parseLines lines

        let neighbourCountSets = Array.init 9 (fun i -> i, HashSet<_>())

        grid
        |> Grid.iteri (fun r c adjacentRollCount ->
            adjacentRollCount
            |> Option.iter (fun adjacentRollCount ->
                neighbourCountSets[adjacentRollCount] |> snd |> _.Add(r, c) |> ignore))

        let fewestNeighbours () =
            neighbourCountSets |> Array.find (fun (_, s) -> s.Count > 0)

        let remove (r, c) =
            let reduceAdjacentRollsCount (r', c') =
                neighbourCountSets
                |> Array.iter (fun (i, s) ->
                    if s.Remove(r', c') then
                        neighbourCountSets[i - 1] |> snd |> _.Add(r', c') |> ignore)

            neighbourCountSets |> Array.iter (fun (i, s) -> s.Remove(r, c) |> ignore)
            (r, c) |> grid.IndexesAdjacentTo |> Array.iter reduceAdjacentRollsCount

        let count () =
            neighbourCountSets |> Array.sumBy (snd >> _.Count)

        let initialCount = count ()

        while fst (fewestNeighbours ()) < 4 do
            fewestNeighbours () |> snd |> Seq.head |> remove

        initialCount - count ()

module Test =

    open Expecto
    open Swensen.Unquote

    let all =
        testList "Day04" [
            let sampleInput = [|
                "..@@.@@@@."
                "@@@.@.@.@@"
                "@@@@@.@.@@"
                "@.@@@@..@."
                "@@.@@@@.@@"
                ".@@@@@@@.@"
                ".@.@.@.@@@"
                "@.@@@.@@@@"
                ".@@@@@@@@."
                "@.@.@@@.@."
            |]

            testList "PartOne" [
                testCase "solve works with sample input" (fun _ -> test <@ PartOne.solve sampleInput = 13 @>)
            ]

            testList "PartTwo" [
                testCase "solve works with sample input" (fun _ -> test <@ PartTwo.solve sampleInput = 43 @>)
            ]
        ]

let dayFns = {
    Tests = Test.all
    UtilTests = []
    PartOne = PartOne.solve >> int64
    PartTwo = PartTwo.solve >> int64
}
