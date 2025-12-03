module Day03

module PartOne =

    let parseLine = Seq.map (_.ToString() >> int) >> Seq.toArray

    let largestJoltage (bank: int array) =
        let l = bank.Length
        let rest, lastJoltage = bank[0 .. l - 3], 10 * bank[l - 2] + bank[l - 1]

        (rest, (lastJoltage, max bank[l - 2] bank[l - 1]))
        ||> Array.foldBack (fun battery (largestJ, largestB) ->
            let j = battery * 10 + largestB
            let newJ = if j > largestJ then j else largestJ
            let newB = if battery > largestB then battery else largestB
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
