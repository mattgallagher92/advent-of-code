module Day20

open System

let parse (lines: string array) =
    lines |> Array.map Seq.toArray |> array2D

type Cheat = { From: int * int; To: int * int; Saving: int }

module PartOne =

    /// Returns track positions, ordered from start position to end position.
    let orderedTrackPositions map =
        let startPos = map |> Array2D.findIndex ((=) 'S')
        let endPos = map |> Array2D.findIndex ((=) 'E')

        let secondPos =
            startPos
            |> Array2D.adjacentIndexes map
            |> Array.find (fun pos -> pos ||> Array2D.get map <> '#')

        (startPos, Some secondPos)
        |> Array.unfold (fun (prev: int * int, curr: (int * int) option) ->
            curr
            |> Option.map (fun c ->
                let next =
                    c
                    |> Array2D.adjacentIndexes map
                    |> Array.tryFind (fun pos -> pos <> prev && pos ||> Array2D.get map <> '#')

                prev, (c, next)))
        |> fun ps -> [| yield! ps; endPos |]

    let allCheats map =
        map
        |> orderedTrackPositions
        |> Array.indexed
        // TODO: this is an n^2 op; consider precomputing lookups.
        |> fun xs -> Array.allPairs xs xs
        |> Array.filter (fun ((d1, p1), (d2, p2)) -> d2 > d1 + 2 && Pair.ortholinearDistance p1 p2 = 2)
        |> Array.map (fun ((d1, p1), (d2, p2)) -> { From = p1; To = p2; Saving = d2 - d1 - 2 })

    let solve lines =
        lines
        |> parse
        |> allCheats
        |> Array.filter (fun c -> c.Saving > 99)
        |> Array.length

module PartTwo =

    let solve (lines: string array) = -1

module Test =

    open Expecto
    open Swensen.Unquote

    let all =
        testList "Day20" [
            let sampleInput = [|
                "###############"
                "#...#...#.....#"
                "#.#.#.#.#.###.#"
                "#S#...#.#.#...#"
                "#######.#.#.###"
                "#######.#.#...#"
                "#######.#.###.#"
                "###..E#...#...#"
                "###.#######.###"
                "#...###...#...#"
                "#.#####.#.###.#"
                "#.#...#.#.#...#"
                "#.#.#.#.#.#.###"
                "#...#...#...###"
                "###############"
            |]

            testList "PartOne" [
                testCase "allCheats works with sample input" (fun _ ->
                    let cheats = PartOne.allCheats (parse sampleInput)
                    let expected = [| 2, 14; 4, 14; 6, 2; 8, 4; 10, 2; 12, 3; 20, 1; 36, 1; 38, 1; 40, 1; 64, 1 |]
                    test <@ cheats |> Array.countBy _.Saving |> Array.sortBy fst = expected @>)
            ]

            testList "PartTwo" [
                testCase "solve works with sample input" (fun _ -> test <@ PartTwo.solve sampleInput = -1 @>)
            ]
        ]

let dayFns = {
    Tests = Test.all
    UtilTests = []
    PartOne = PartOne.solve >> int64
    PartTwo = PartTwo.solve >> int64
}
