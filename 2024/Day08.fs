module Day8

open System

let parse (lines: string array) = lines |> array2D

module PartOne =

    let antinodesProducedBy ((r1, c1), (r2, c2)) =
        let dr, dc = r2 - r1, c2 - c1
        (r1 - dr, c1 - dc), (r2 + dr, c2 + dc)

    let antinodeLocations map =
        map
        |> Array2D.indexed
        |> Array.filter (fun (_, v) -> Char.IsLetter v || Char.IsDigit v)
        |> Array.groupBy snd
        |> Array.map snd
        |> Array.collect (
            Array.map fst
            >> (fun locations -> Array.allPairs locations locations |> Array.filter (fun (x, y) -> x <> y))
            >> Array.collect (antinodesProducedBy >> Pair.asArray)
        )
        |> Array.distinct
        |> Array.filter (Array2D.isWithinBounds map)

    let solve (lines: string array) =
        let map = lines |> parse

        map |> antinodeLocations |> Array.length

module PartTwo =

    let solve (lines: string array) = -1L

module Test =

    open Expecto
    open Swensen.Unquote

    let all =
        testList "Day8" [
            let sampleInput = [|
                "............"
                "........0..."
                ".....0......"
                ".......0...."
                "....0......."
                "......A....."
                "............"
                "............"
                "........A..."
                ".........A.."
                "............"
                "............"
            |]

            let sampleMap = parse sampleInput

            testList "PartOne" [
                testCase "antinodesProducedBy works with sample input" (fun _ ->
                    test <@ PartOne.antinodesProducedBy ((3, 4), (5, 5)) = ((1, 3), (7, 6)) @>)

                testCase "antinodeLocations works with sample input" (fun _ ->
                    let expected = [|
                        0, 6
                        0, 11
                        1, 3
                        2, 4
                        2, 10
                        3, 2
                        4, 9
                        5, 1
                        5, 6
                        6, 3
                        7, 0
                        7, 7
                        10, 10
                        11, 10
                    |]

                    test <@ PartOne.antinodeLocations sampleMap |> Array.sort = expected @>)

                testCase "solve works with sample input" (fun _ -> test <@ PartOne.solve sampleInput = 14 @>)
            ]

            testList "PartTwo" [
                testCase "solve works with sample input" (fun _ -> test <@ PartTwo.solve sampleInput = -1 @>)
            ]
        ]

let dayFns = {
    Tests = Test.all
    ReadInput = System.IO.File.ReadAllLines
    PartOne = PartOne.solve >> int64
    PartTwo = PartTwo.solve >> int64
}
