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

    let parseLine = Seq.map (_.ToString() >> int64) >> Seq.toArray

    /// Given an array of non-negative integers, returns a pair whose second element is the largest item from it, and
    /// whose first element is the index of that item's first occurrence.
    let largestItem (arr: int64 array) =
        ((0, -1, -1L), arr)
        ||> Array.fold (fun (currentIx, largestIx, largestX) x ->
            if x > largestX then
                currentIx + 1, currentIx, x
            else
                currentIx + 1, largestIx, largestX)
        |> fun (_, ix, x) -> ix, x

    let largestJoltage (bank: int64 array) =
        /// `n`: the index of the digit that we're currently looking for, 0-based, counting from the right. That is to
        /// say, what will eventually be the digit representing the number of 10^n's in the joltage.
        /// `i_n'`: the index of digit (n + 1) within bank.
        ///
        /// Returns the total joltage found.
        let rec inner n i_n' runningJoltage =
            if n < 0 then
                runningJoltage
            else
                let i, d = largestItem bank[i_n' + 1 .. bank.Length - 1 - n]
                inner (n - 1) (i_n' + 1 + i) (runningJoltage * 10L + d)

        // Find biggest number at least 12 from end; furthest left wins.
        // Then biggest number after it at least 11 from end; furthest left wins.
        // Etc.
        inner 11 -1 0L

    let solve (lines: string array) =
        lines |> Array.sumBy (parseLine >> largestJoltage)

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
                testCase "largest joltage of 987654321111111 is 987654321111" (fun _ ->
                    test <@ PartTwo.largestJoltage (PartTwo.parseLine "987654321111111") = 987654321111L @>)

                testCase "solve works with sample input" (fun _ ->
                    test <@ PartTwo.solve sampleInput = 3121910778619L @>)
            ]
        ]

let dayFns = {
    Tests = Test.all
    UtilTests = []
    PartOne = PartOne.solve >> int64
    PartTwo = PartTwo.solve >> int64
}
