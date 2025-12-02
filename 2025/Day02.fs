module Day02

open System

type Int64 with
    member this.NumDigits =
        Seq.initInfinite (fun i -> Math.integerPower 10L i)
        |> Seq.findIndex (fun p -> p > this)

type Range = { Lower: int64; Upper: int64 }

let parse (input: string) =
    input.Split ','
    |> Array.map (fun s ->
        s.Split '-'
        |> fun parts -> {
            Lower = int64 parts[0]
            Upper = int64 parts[1]
        })

type SameNumberOfDigitsRange = { Range: Range; NumDigits: int }

let sameNumberOfDigitsRanges range =
    [| range.Lower.NumDigits .. range.Upper.NumDigits |]
    |> Array.map (fun n -> {
        Range = {
            Lower = max range.Lower (Math.integerPower 10L (n - 1))
            Upper = min range.Upper (Math.integerPower 10L n - 1L)
        }
        NumDigits = n
    })

let repeats numRepeats sameNumberOfDigitsRange =
    match Math.DivRem(sameNumberOfDigitsRange.NumDigits, numRepeats) with
    | struct (q, 0) ->
        let divisorOfRepeats =
            let x = Math.integerPower 10L q
            [| 0 .. (numRepeats - 1) |] |> Array.sumBy (Math.integerPower x)

        let ql, rl = Math.DivRem(sameNumberOfDigitsRange.Range.Lower, divisorOfRepeats)
        let qu, _ = Math.DivRem(sameNumberOfDigitsRange.Range.Upper, divisorOfRepeats)
        let l = if rl = 0 then ql else ql + 1L
        [| l..qu |] |> Array.map ((*) divisorOfRepeats)
    | _ -> [||]

module PartOne =

    let solve (input: string) =
        input
        |> parse
        |> Array.collect (sameNumberOfDigitsRanges >> Array.filter (fun x -> x.NumDigits % 2 = 0))
        |> Array.collect (repeats 2)
        |> Array.sum

module PartTwo =

    let repeats x =
        match x.NumDigits with
        | 01 -> [||]
        | 02 -> [| 2 |]
        | 03 -> [| 3 |]
        | 04 -> [| 2; 4 |]
        | 05 -> [| 5 |]
        | 06 -> [| 2; 3; 6 |]
        | 07 -> [| 7 |]
        | 08 -> [| 2; 4; 8 |]
        | 09 -> [| 3; 9 |]
        | 10 -> [| 2; 5; 10 |]
        | n -> failwith $"Expected no more than 10 digits, but found %i{n}"
        |> Array.collect (fun n -> repeats n x)
        |> Array.distinct

    let solve (input: string) =
        input
        |> parse
        |> Array.collect sameNumberOfDigitsRanges
        |> Array.collect repeats
        |> Array.sum

module Test =

    open Expecto
    open Swensen.Unquote

    let all =
        testList "Day02" [

            testCase "repeats 3 100-115 returns 111" (fun _ ->
                test
                    <@
                        repeats 3 {
                            NumDigits = 3
                            Range = { Lower = 100L; Upper = 115L }
                        } = [| 111L |]
                    @>)

            let sampleInput =
                "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

            testList "PartOne" [
                testCase "solve works with sample input" (fun _ -> test <@ PartOne.solve sampleInput = 1227775554L @>)
            ]

            testList "PartTwo" [

                testCase "repeats 222220-222224 returns 222222 once" (fun _ ->
                    test
                        <@
                            PartTwo.repeats {
                                NumDigits = 6
                                Range = { Lower = 222220L; Upper = 222224L }
                            } = [| 222222L |]
                        @>)

                testCase "solve works with sample input" (fun _ -> test <@ PartTwo.solve sampleInput = 4174379265L @>)
            ]
        ]

let dayFns = {
    Tests = Test.all
    UtilTests = []
    PartOne = Array.head >> PartOne.solve >> int64
    PartTwo = Array.head >> PartTwo.solve >> int64
}
