module Day8

open System

let parse (lines: string array) = lines |> array2D

let antinodeLocations antinodesProducedByPair map =
    map
    |> Array2D.indexed
    |> Array.filter (fun (_, v) -> Char.IsLetter v || Char.IsDigit v)
    |> Array.groupBy snd
    |> Array.map snd
    |> Array.collect (
        Array.map fst
        >> (fun locations -> Array.allPairs locations locations |> Array.filter (fun (x, y) -> x <> y))
        >> Array.collect antinodesProducedByPair
    )
    |> Array.distinct
    |> Array.filter (Array2D.isWithinBounds map)

let solve antinodesProducedByPair lines =
    let map = lines |> parse

    map |> antinodeLocations antinodesProducedByPair |> Array.length

module PartOne =

    let antinodesProducedByPair ((r1, c1), (r2, c2)) =
        let dr, dc = r2 - r1, c2 - c1
        [| r1 - dr, c1 - dc; r2 + dr, c2 + dc |]

    let solve (lines: string array) = solve antinodesProducedByPair lines

module PartTwo =

    let antinodesProducedByPair map ((r1, c1), (r2, c2)) =
        let dr, dc = r2 - r1, c2 - c1

        let backwards =
            Seq.initInfinite (fun i -> r1 - (i + 1) * dr, c1 - (i + 1) * dc)
            |> Seq.takeWhile (Array2D.isWithinBounds map)
            |> Seq.toArray
            |> Array.rev

        let forwards =
            Seq.initInfinite (fun i -> r1 + i * dr, c1 + i * dc)
            |> Seq.takeWhile (Array2D.isWithinBounds map)
            |> Seq.toArray

        Array.append backwards forwards

    let solve (lines: string array) =
        let map = parse lines
        solve (antinodesProducedByPair map) lines

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
                    test <@ PartOne.antinodesProducedByPair ((3, 4), (5, 5)) = [| 1, 3; 7, 6 |] @>)

                testCase "antinodeLocations works with sample input" (fun _ ->
                    let result =
                        antinodeLocations PartOne.antinodesProducedByPair sampleMap |> Array.sort

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

                    test <@ result = expected @>)

                testCase "solve works with sample input" (fun _ -> test <@ PartOne.solve sampleInput = 14 @>)
            ]

            testList "PartTwo" [
                testCase "solve works with sample input" (fun _ -> test <@ PartTwo.solve sampleInput = 34 @>)
            ]
        ]

let dayFns = {
    Tests = Test.all
    ReadInput = System.IO.File.ReadAllLines
    PartOne = PartOne.solve >> int64
    PartTwo = PartTwo.solve >> int64
}
