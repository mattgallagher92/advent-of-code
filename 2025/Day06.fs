module Day06

module PartOne =

    let solve (lines: string array) =
        let splitLines =
            lines |> Array.map (fun line -> line.Split(' ') |> Array.filter ((<>) ""))

        let operations =
            splitLines
            |> Array.last
            |> Array.map (function
                | "+" -> fun a b -> a + b + 0L
                | "*" -> fun a b -> a * b * 1L
                | s -> failwith $"Invalid operation string %s{s}")

        let numberLines =
            splitLines[.. splitLines.Length - 2] |> Array.map (Array.map int64)

        (numberLines |> Array.head, numberLines |> Array.tail)
        ||> Array.fold (fun state line -> (operations, state, line) |||> Array.map3 (fun o s l -> o s l))
        |> Array.sum

module PartTwo =

    let solve (lines: string array) = -1

module Test =

    open Expecto
    open Swensen.Unquote

    let all =
        testList "Day06" [
            let sampleInput = [| "123 328  51 64 "; " 45 64  387 23 "; "  6 98  215 314"; "*   +   *   +  " |]

            testList "PartOne" [
                testCase "solve works with sample input" (fun _ -> test <@ PartOne.solve sampleInput = 4277556 @>)
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
