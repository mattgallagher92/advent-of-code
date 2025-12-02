module Day02

open System

type Int64 with
    member this.NumDigits =
        Seq.initInfinite (fun i -> Math.integerPower 10L i)
        |> Seq.findIndex (fun p -> p > this)

module PartOne =

    type EvenDigitRange =
        {
            Lower: int64
            Upper: int64
            NumDigits: int
        }

        member this.TwiceRepeats =
            let divisorOfRepeats = Math.integerPower 10L (this.NumDigits / 2) + 1L
            let ql, rl = Math.DivRem(this.Lower, divisorOfRepeats)
            let qu, _ = Math.DivRem(this.Upper, divisorOfRepeats)
            let l = if rl = 0 then ql else ql + 1L
            [| l..qu |] |> Array.map ((*) divisorOfRepeats)

    type Range =
        {
            Lower: int64
            Upper: int64
        }

        member this.EvenDigitRanges =
            [| this.Lower.NumDigits .. this.Upper.NumDigits |]
            |> Array.filter (fun n -> n % 2 = 0)
            |> Array.map (fun n ->
                {
                    Lower = max this.Lower (Math.integerPower 10L (n - 1))
                    Upper = min this.Upper (Math.integerPower 10L n - 1L)
                    NumDigits = n
                }
            )

    let parse (input: string) =
        input.Split ','
        |> Array.map (fun s ->
            s.Split '-'
            |> fun parts ->
                {
                    Lower = int64 parts[0]
                    Upper = int64 parts[1]
                }
        )

    let solve (input: string) =
        input
        |> parse
        |> Array.collect _.EvenDigitRanges
        |> Array.collect _.TwiceRepeats
        |> Array.sum

module PartTwo =

    let solve (input: string) = -1

module Test =

    open Expecto
    open Swensen.Unquote

    let all =
        testList
            "Day02"
            [
                let sampleInput =
                    "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

                testList
                    "PartOne"
                    [
                        testCase
                            "solve works with sample input"
                            (fun _ -> test <@ PartOne.solve sampleInput = 1227775554 @>)
                    ]

                testList
                    "PartTwo"
                    [ testCase "solve works with sample input" (fun _ -> test <@ PartTwo.solve sampleInput = -1 @>) ]
            ]

let dayFns =
    {
        Tests = Test.all
        UtilTests = []
        PartOne = Array.head >> PartOne.solve >> int64
        PartTwo = Array.head >> PartTwo.solve >> int64
    }
