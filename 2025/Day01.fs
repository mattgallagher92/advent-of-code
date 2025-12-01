module Day01

let modulo m x = (x % m + m) % m

module PartOne =

    let parseLine (line: string) =
        match line[0], Int32.tryParse line[1..] with
        | 'L', Some i -> -i
        | 'R', Some i -> i
        | _ -> failwith $"Invalid line %s{line}"

    let applyRotation state rotation = state + rotation |> modulo 100

    let solve (lines: string array) =
        lines
        |> Array.map parseLine
        |> Array.scan applyRotation 50
        |> Array.filter ((=) 0)
        |> Array.length

module PartTwo =

    let solve (lines: string array) = -1

module Test =

    open Expecto
    open Swensen.Unquote

    let all =
        testList "DayN" [
            let sampleInput = [| "L68"; "L30"; "R48"; "L5"; "R60"; "L55"; "L1"; "L99"; "R14"; "L82" |]

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
