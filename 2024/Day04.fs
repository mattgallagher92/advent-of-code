module Day4

let itemLocations item puzzle =
    puzzle
    |> Array.map (Array.findAllIndexes ((=) item))
    |> Array.indexed
    |> Array.collect (fun (row, cols) -> cols |> Array.map (fun col -> row, col))

let isWithinPuzzleBounds puzzle (row, col) =
    let width = puzzle |> Array.head |> Array.length
    let height = puzzle.Length
    -1 < row && row < height && -1 < col && col < width

let indexesToChars (puzzle: char array array) indexes =
    indexes |> Array.map (fun (row, col) -> puzzle.[row].[col])

module PartOne =

    let lengthFourStringsStartingFrom puzzle (row, col) =
        let isWithinBounds = isWithinPuzzleBounds puzzle

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

        let lengthFourStringsStartingFromAnX =
            ('X', puzzle)
            ||> itemLocations
            |> Array.collect (lengthFourStringsStartingFrom puzzle)
            |> Array.map (indexesToChars puzzle)

        lengthFourStringsStartingFromAnX
        |> Array.filter ((=) [| 'X'; 'M'; 'A'; 'S' |])
        |> Array.length

module PartTwo =

    let solve (lines: string array) =
        let puzzle = lines |> Array.map Seq.toArray
        let isWithinBounds = isWithinPuzzleBounds puzzle

        ('A', puzzle)
        ||> itemLocations
        |> Array.map (fun (r, c) ->
            let tlToBr = [| r - 1, c - 1; r, c; r + 1, c + 1 |]
            let blToTr = [| r + 1, c - 1; r, c; r - 1, c + 1 |]
            tlToBr, blToTr)
        |> Array.filter (fun (a, b) -> a |> Array.append b |> Array.forall isWithinBounds)
        |> Array.map (fun (a, b) -> indexesToChars puzzle a, indexesToChars puzzle b)
        |> Array.filter (fun (a, b) ->
            (a = [| 'M'; 'A'; 'S' |] || a = [| 'S'; 'A'; 'M' |])
            && (b = [| 'M'; 'A'; 'S' |] || b = [| 'S'; 'A'; 'M' |]))
        |> Array.length

module Test =
    open Expecto
    open Swensen.Unquote

    let all =
        testList "Day 4" [
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

            testCase "PartTwo.solve works with sample input" (fun _ -> test <@ PartTwo.solve sampleInput = 9 @>)
        ]

let dayFns = {
    Tests = Test.all
    UtilTests = []
    PartOne = PartOne.solve >> int64
    PartTwo = PartTwo.solve >> int64
}
