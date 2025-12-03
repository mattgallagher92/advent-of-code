module Day03

module PartOne =

    let parseLine = Seq.map (_.ToString() >> int) >> Seq.toArray

    let largestJoltage bank =
        (bank, (0, None))
        ||> Array.foldBack (fun battery (largestJoltageSoFar, largestBatterySoFar) ->
            match largestBatterySoFar with
            | None -> 0, Some battery
            | Some lb ->
                let j = battery * 10 + lb

                let newJ = if j > largestJoltageSoFar then j else largestJoltageSoFar
                let newB = if battery > lb then Some battery else largestBatterySoFar
                newJ, newB)
        |> fst

    let solve (lines: string array) =
        lines |> Array.sumBy (parseLine >> largestJoltage)

module PartTwo =

    let solve (lines: string array) = -1

module Test =

    open Expecto
    open Swensen.Unquote

    let all =
        testList "Day03" [
            let sampleInput = [| "987654321111111"; "811111111111119"; "234234234234278"; "818181911112111" |]

            testList "PartOne" [
                testCase "solve works with sample input" (fun _ -> test <@ PartOne.solve sampleInput = 357 @>)
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
