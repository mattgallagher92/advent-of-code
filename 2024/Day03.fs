module Day3

open System
open System.Text.RegularExpressions

type MultiplyInstruction = { X: int; Y: int }

module PartOne =

    let regex = Regex @"mul\((\d{1,3}),(\d{1,3})\)"

    let parse (lines: string array) =
        lines
        |> Array.collect (fun line ->
            regex.Matches line
            |> Seq.toArray
            |> Array.map (fun m ->
                let g1 = m.Groups.Item 1 |> _.Value |> Int32.Parse
                let g2 = m.Groups.Item 2 |> _.Value |> Int32.Parse
                { X = g1; Y = g2 }))

module Test =
    open Expecto
    open Swensen.Unquote

    let all =
        testList "Day 3" [
            let sampleInput =
                "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

            testCase "PartOne.parse works with sample input" (fun _ ->
                let expected = [| { X = 2; Y = 4 }; { X = 5; Y = 5 }; { X = 11; Y = 8 }; { X = 8; Y = 5 } |]
                test <@ PartOne.parse [| sampleInput |] = expected @>)
        ]
