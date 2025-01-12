module Day20

let parse (lines: string array) =
    lines |> Array.map Seq.toArray |> array2D

type Cheat = { From: int * int; To: int * int; Saving: int }

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

let allCheats relativePositionsToCheck map =
    let ixdTrackPositions = map |> orderedTrackPositions |> Array.indexed
    let posToIx = ixdTrackPositions |> Array.map (fun (i, p) -> p, i) |> dict

    ixdTrackPositions
    |> Array.collect (fun (d0, p0) ->
        relativePositionsToCheck
        |> Array.choose (fun rp ->
            let p = rp |> Pair.add p0
            let x = Pair.ortholinearDistance p0 p

            posToIx.TryGet p
            |> Option.bind (fun d -> if d > d0 + x then Some((d0, p0), (d, p)) else None)))
    |> Array.map (fun ((d1, p1), (d2, p2)) -> {
        From = p1
        To = p2
        Saving = d2 - d1 - Pair.ortholinearDistance p1 p2
    })

module PartOne =

    let allCheats =
        allCheats [| -2, 0; -1, -1; -1, +1; 0, -2; 0, +2; +1, -1; +1, +1; +2, 0 |]

    let solve lines =
        lines
        |> parse
        |> allCheats
        |> Array.filter (fun c -> c.Saving > 99)
        |> Array.length

module PartTwo =

    let allCheats =
        Array.allPairs [| -20 .. 20 |] [| -20 .. 20 |]
        |> Array.filter (fun p ->
            let d = Pair.ortholinearDistance p (0, 0)
            0 < d && d < 21)
        |> allCheats
        >> Array.distinctBy (fun c -> c.From, c.To)

    let solve lines =
        lines
        |> parse
        |> allCheats
        |> Array.filter (fun c -> c.Saving > 99)
        |> Array.length

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
                    test <@ cheats |> Array.countBy (fun c -> c.Saving) |> Array.sortBy fst = expected @>)
            ]

            testList "PartTwo" [
                testCase "allCheats works with sample input" (fun _ ->
                    let cheats = PartTwo.allCheats (parse sampleInput)

                    let expected = [|
                        50, 32
                        52, 31
                        54, 29
                        56, 39
                        58, 25
                        60, 23
                        62, 20
                        64, 19
                        66, 12
                        68, 14
                        70, 12
                        72, 22
                        74, 4
                        76, 3
                    |]

                    test
                        <@
                            cheats
                            |> Array.filter (fun c -> c.Saving > 49)
                            |> Array.countBy _.Saving
                            |> Array.sortBy fst = expected
                        @>)
            ]
        ]

let dayFns = {
    Tests = Test.all
    UtilTests = []
    PartOne = PartOne.solve >> int64
    PartTwo = PartTwo.solve >> int64
}
