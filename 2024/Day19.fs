module Day19

module PartOne =

    let solve (lines: string array) =
        let towels = lines[0].Split ", "
        let desiredPatterns = lines[2..]
        let maxDesiredLength = desiredPatterns |> Array.map _.Length |> Array.max

        let rec inner n m =
            printfn $"%i{n}"

            if n <= maxDesiredLength then
                let lengthNPatterns =
                    towels
                    |> Array.map (fun t ->
                        if t.Length = n then
                            set [ t ]
                        else
                            m
                            |> Map.tryFind (n - t.Length)
                            |> Option.map (Set.map (fun p -> $"%s{p}{t}"))
                            |> Option.defaultValue Set.empty)
                    |> Set.unionMany

                m |> Map.add n lengthNPatterns |> inner (n + 1)
            else
                m

        let patternMap = inner 0 Map.empty

        desiredPatterns
        |> Array.filter (fun p -> patternMap[p.Length].Contains p)
        |> Array.length

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
