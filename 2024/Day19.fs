module Day19

module PartOne =

    open System.Collections.Generic

    let solve (lines: string array) =
        let towels = lines[0].Split ", "
        let desiredPatterns = lines[2..]

        let cache = towels |> Array.map (fun t -> KeyValuePair(t, true)) |> Dictionary

        let rec isPossible pattern =
            cache.TryGet pattern
            |> Option.defaultWith (fun _ ->
                let b =
                    towels
                    |> Seq.map (fun t -> pattern.StartsWith t && isPossible (pattern[t.Length ..]))
                    |> Seq.tryFind id
                    |> Option.defaultValue false

                cache[pattern] <- b
                b)

        desiredPatterns |> Array.filter isPossible |> Array.length

module PartTwo =

    let solve (lines: string array) = -1

module Test =

    open Expecto
    open Swensen.Unquote

    let all =
        testList "Day19" [
            let sampleInput = [|
                "r, wr, b, g, bwu, rb, gb, br"
                ""
                "brwrr"
                "bggr"
                "gbbr"
                "rrbgbr"
                "ubwu"
                "bwurrg"
                "brgr"
                "bbrgwb"
            |]

            testList "PartOne" [
                testCase "solve works with sample input" (fun _ -> test <@ PartOne.solve sampleInput = 6 @>)
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
