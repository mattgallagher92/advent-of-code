module Day2

open System

let parse (lines: string array) =
    lines
    |> Array.map (fun line ->
        line.Split " "
        |> FsToolkit.ErrorHandling.Array.traverseOptionM Int32.tryParse
        |> Option.defaultWith (fun _ -> failwith $"Invalid line %s{line}"))

type ReportClassificatoin =
    | Safe
    | Unsafe

let solve classify (lines: string array) =
    lines |> parse |> Array.countBy classify |> Array.find (fst >> (=) Safe) |> snd

module PartOne =

    let classify (report: int array) =
        let differences = report |> Array.pairwise |> Array.map (fun (a, b) -> b - a)

        let changesGradually =
            differences |> Array.forall (fun x -> 0 < Math.Abs x && Math.Abs x < 4)

        let changeIsMonotonic =
            differences |> Array.map Math.Sign |> Array.distinct |> Array.length = 1

        if changesGradually && changeIsMonotonic then
            Safe
        else
            Unsafe

    let solve = solve classify

module PartTwo =

    let classify (report: int array) =
        [|
            for i in 0 .. report.Length - 1 do
                report |> Array.removeAt i
        |]
        |> Array.exists (PartOne.classify >> (=) Safe)
        |> function
            | true -> Safe
            | false -> Unsafe

    let solve = solve classify

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

            let sampleReport1 = [| 7; 6; 4; 2; 1 |]
            let sampleReport2 = [| 1; 2; 7; 8; 9 |]
            let sampleReport3 = [| 9; 7; 6; 2; 1 |]
            let sampleReport4 = [| 1; 3; 2; 4; 5 |]
            let sampleReport5 = [| 8; 6; 4; 4; 1 |]
            let sampleReport6 = [| 1; 3; 6; 7; 9 |]

            let sampleReports = [|
                sampleReport1
                sampleReport2
                sampleReport3
                sampleReport4
                sampleReport5
                sampleReport6
            |]

            testCase "parse works on sample input" (fun _ -> test <@ parse sampleInput = sampleReports @>)

            testCase "PartOne.classify works on sample input" (fun _ ->
                test <@ PartOne.classify sampleReport1 = Safe @>
                test <@ PartOne.classify sampleReport2 = Unsafe @>
                test <@ PartOne.classify sampleReport3 = Unsafe @>
                test <@ PartOne.classify sampleReport4 = Unsafe @>
                test <@ PartOne.classify sampleReport5 = Unsafe @>
                test <@ PartOne.classify sampleReport6 = Safe @>)

            testCase "PartOne.solve works on sample input" (fun _ -> test <@ PartOne.solve sampleInput = 2 @>)

            testCase "PartTwo.classify works on sample input" (fun _ ->
                test <@ PartTwo.classify sampleReport1 = Safe @>
                test <@ PartTwo.classify sampleReport2 = Unsafe @>
                test <@ PartTwo.classify sampleReport3 = Unsafe @>
                test <@ PartTwo.classify sampleReport4 = Safe @>
                test <@ PartTwo.classify sampleReport5 = Safe @>
                test <@ PartTwo.classify sampleReport6 = Safe @>)

            testCase "PartTwo.solve works on sample input" (fun _ -> test <@ PartTwo.solve sampleInput = 4 @>)
        ]
