module Day2

module PartOne =
    open FsToolkit.ErrorHandling

    let parse (lines: string array) =
        lines
        |> Array.map (fun line ->
            line.Split(" ")
            |> Array.traverseOptionM Int32.tryParse
            |> Option.defaultWith (fun _ -> failwith $"Invalid line %s{line}"))

    let classify (report: int array) = ()

    let solve (lines: string array) = ()

module Test =
    open Expecto
    open Swensen.Unquote

    let all =
        testList "Day 2" [
            let sampleInput = [|
                "7 6 4 2 1"
                "1 2 7 8 9"
                "9 7 6 2 1"
                "1 3 2 4 5"
                "8 6 4 4 1"
                "1 3 6 7 9"
            |]

            testCase "PartOne.parse works" (fun _ ->
                let expected = [|
                    [| 7; 6; 4; 2; 1 |]
                    [| 1; 2; 7; 8; 9 |]
                    [| 9; 7; 6; 2; 1 |]
                    [| 1; 3; 2; 4; 5 |]
                    [| 8; 6; 4; 4; 1 |]
                    [| 1; 3; 6; 7; 9 |]
                |]

                test <@ PartOne.parse sampleInput = expected @>)
        ]
