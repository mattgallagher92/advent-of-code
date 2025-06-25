module Day25

module PartOne =

    let boolToInt =
        function
        | true -> 1
        | false -> 0

    let pinHeights schematic =
        schematic
        |> Array2D.columns
        |> Array.map (fun col -> col[1..5] |> Array.sumBy ((=) '#' >> boolToInt))

    let solve (lines: string array) =
        lines
        |> Array.filter ((<>) "")
        |> Array.chunkBySize 7
        |> Array.map array2D
        |> Array.partition (fun schematic -> Array2D.get schematic 0 0 = '#')
        ||> Array.allPairs
        |> Array.sumBy (fun (lock, key) ->
            (pinHeights lock, pinHeights key)
            ||> Array.forall2 (fun l k -> l + k < 6)
            |> boolToInt)

module PartTwo =

    let solve (lines: string array) = -1

module Test =

    open Expecto
    open Swensen.Unquote

    let all =
        testList "Day25" [
            let sampleInput = [|
                "#####"
                ".####"
                ".####"
                ".####"
                ".#.#."
                ".#..."
                "....."
                ""
                "#####"
                "##.##"
                ".#.##"
                "...##"
                "...#."
                "...#."
                "....."
                ""
                "....."
                "#...."
                "#...."
                "#...#"
                "#.#.#"
                "#.###"
                "#####"
                ""
                "....."
                "....."
                "#.#.."
                "###.."
                "###.#"
                "###.#"
                "#####"
                ""
                "....."
                "....."
                "....."
                "#...."
                "#.#.."
                "#.#.#"
                "#####"
            |]

            testList "PartOne" [
                testCase "solve works with sample input" (fun _ -> test <@ PartOne.solve sampleInput = 3 @>)
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
