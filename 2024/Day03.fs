module Day3

open System
open System.Text.RegularExpressions

type MultiplyInstruction = { X: int; Y: int }

module PartOne =

    let regex = Regex @"mul\((\d{1,3}),(\d{1,3})\)"

    let parseMatch (m: Match) =
        let g1 = m.Groups.Item 1 |> _.Value |> Int32.Parse
        let g2 = m.Groups.Item 2 |> _.Value |> Int32.Parse
        { X = g1; Y = g2 }

    let parse (lines: string array) =
        lines
        |> Array.collect (fun line -> regex.Matches line |> Seq.toArray |> Array.map parseMatch)

    let solve (lines: string array) =
        lines |> parse |> Array.sumBy (fun m -> m.X * m.Y)

module PartTwo =

    type Instruction =
        | Enable
        | Disable
        | Multiply of MultiplyInstruction

    let regex = Regex @"mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\)"

    let parse (lines: string array) =
        lines
        |> Array.collect (fun line ->
            regex.Matches line
            |> Seq.toArray
            |> Array.map (fun m ->
                match m.Value |> Seq.toList with
                | 'm' :: _ -> PartOne.parseMatch m |> Multiply
                | 'd' :: 'o' :: 'n' :: _ -> Disable
                | 'd' :: 'o' :: _ -> Enable
                | _ -> failwith $"Unexpected match: %s{m.Value}"))

module Test =
    open Expecto
    open Swensen.Unquote

    let all =
        testList "Day 3" [
            testList "PartOne" [
                let sampleInput =
                    "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

                testCase "PartOne.parse works with sample input" (fun _ ->
                    let expected = [| { X = 2; Y = 4 }; { X = 5; Y = 5 }; { X = 11; Y = 8 }; { X = 8; Y = 5 } |]
                    test <@ PartOne.parse [| sampleInput |] = expected @>)

                testCase "PartOne.solve works with sample input" (fun _ ->
                    test <@ PartOne.solve [| sampleInput |] = 161 @>)
            ]

            testList "PartTwo" [
                let sampleInput =
                    "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

                testCase "PartTwo.parse works with sample input" (fun _ ->
                    let expected = [|
                        PartTwo.Multiply { X = 2; Y = 4 }
                        PartTwo.Disable
                        PartTwo.Multiply { X = 5; Y = 5 }
                        PartTwo.Multiply { X = 11; Y = 8 }
                        PartTwo.Enable
                        PartTwo.Multiply { X = 8; Y = 5 }
                    |]

                    test <@ PartTwo.parse [| sampleInput |] = expected @>)
            ]
        ]
