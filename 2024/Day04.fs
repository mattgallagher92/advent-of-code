module Day4

module PartOne =

    let lengthFourStringsStartingFrom puzzle (row, col) =
        let isWithinBounds (row, col) =
            let width = puzzle |> Array.head |> Array.length
            let height = puzzle.Length
            -1 < row && row < height && -1 < col && col < width

        [|
            let up = [| row; row - 1; row - 2; row - 3 |]
            let horizontal = [| row; row; row; row |]
            let down = [| row; row + 1; row + 2; row + 3 |]

            let left = [| col; col - 1; col - 2; col - 3 |]
            let vertical = [| col; col; col; col |]
            let right = [| col; col + 1; col + 2; col + 3 |]

            Array.zip up vertical
            Array.zip up right
            Array.zip horizontal right
            Array.zip down right
            Array.zip down vertical
            Array.zip down left
            Array.zip horizontal left
            Array.zip up left
        |]
        |> Array.filter (Array.forall isWithinBounds)

    let solve (lines: string array) =
        let puzzle = lines |> Array.map Seq.toArray

        let xLocations =
            puzzle
            |> Array.map (Array.findAllIndexes ((=) 'X'))
            |> Array.indexed
            |> Array.collect (fun (row, cols) -> cols |> Array.map (fun col -> row, col))

        let lengthFourStringsStartingFromAnX =
            xLocations
            |> Array.collect (lengthFourStringsStartingFrom puzzle)
            |> Array.map (Array.map (fun (row, col) -> puzzle.[row].[col]))

        lengthFourStringsStartingFromAnX
        |> Array.filter ((=) [| 'X'; 'M'; 'A'; 'S' |])
        |> Array.length

module Test =
    open Expecto
    open Swensen.Unquote

    let all =
        testList "Day 4" [
            testList "PartOne" [
                let sampleInput = [|
                    "MMMSXXMASM"
                    "MSAMXMSMSA"
                    "AMXSXMAAMM"
                    "MSAMASMSMX"
                    "XMASAMXAMM"
                    "XXAMMXXAMA"
                    "SMSMSASXSS"
                    "SAXAMASAAA"
                    "MAMMMXMMMM"
                    "MXMXAXMASX"
                |]

                testCase "PartOne.solve works with sample input" (fun _ -> test <@ PartOne.solve sampleInput = 18 @>)
            ]
        ]
