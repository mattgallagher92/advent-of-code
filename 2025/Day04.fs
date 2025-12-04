module Day04

module PartOne =

    let solve (lines: string array) =
        let grid = lines |> Array.map Seq.toArray |> Grid.create

        grid
        |> Grid.indexed
        |> Array.countIf (fun (ix, x) ->
            let adjacentRollsCount = ix |> grid.ItemsAdjacentTo |> Array.countIf ((=) '@')
            x = '@' && adjacentRollsCount < 4)

module PartTwo =

    let solve (lines: string array) = -1

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
                testCase "solve works with sample input" (fun _ -> test <@ PartTwo.solve sampleInput = -1 @>)
            ]
        ]

let dayFns = {
    Tests = Test.all
    UtilTests = []
    PartOne = PartOne.solve >> int64
    PartTwo = PartTwo.solve >> int64
}
