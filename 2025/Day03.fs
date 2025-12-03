module Day03

let parseLine = Seq.map (_.ToString() >> int) >> Seq.toArray

module PartOne =

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

    /// Given an array of non-negative integers, returns a pair whose second element is the largest item from it, and
    /// whose first element is the index of that item's first occurrence.
    let largestItem (arr: int array) =
        ((0, -1, -1), arr)
        ||> Array.fold (fun (currentIx, largestIx, largestX) x ->
            if x > largestX then
                currentIx + 1, currentIx, x
            else
                currentIx + 1, largestIx, largestX)
        |> fun (_, ix, x) -> ix, x

    let largestJoltage (bank: int array) =
        let addToFirst j (i, d) = i + j, d
        // Find biggest number at least 12 from end; furthest left wins.
        let i11, d11 = largestItem bank[0 .. bank.Length - 12]
        // Then biggest number after it at least 11 from end; furthest left wins.
        let i10, d10 = largestItem bank[i11 + 1 .. bank.Length - 11] |> addToFirst (i11 + 1)
        // Etc.
        let i09, d09 = largestItem bank[i10 + 1 .. bank.Length - 10] |> addToFirst (i10 + 1)
        let i08, d08 = largestItem bank[i09 + 1 .. bank.Length - 09] |> addToFirst (i09 + 1)
        let i07, d07 = largestItem bank[i08 + 1 .. bank.Length - 08] |> addToFirst (i08 + 1)
        let i06, d06 = largestItem bank[i07 + 1 .. bank.Length - 07] |> addToFirst (i07 + 1)
        let i05, d05 = largestItem bank[i06 + 1 .. bank.Length - 06] |> addToFirst (i06 + 1)
        let i04, d04 = largestItem bank[i05 + 1 .. bank.Length - 05] |> addToFirst (i05 + 1)
        let i03, d03 = largestItem bank[i04 + 1 .. bank.Length - 04] |> addToFirst (i04 + 1)
        let i02, d02 = largestItem bank[i03 + 1 .. bank.Length - 03] |> addToFirst (i03 + 1)
        let i01, d01 = largestItem bank[i02 + 1 .. bank.Length - 02] |> addToFirst (i02 + 1)
        let _i0, d00 = largestItem bank[i01 + 1 .. bank.Length - 01] |> addToFirst (i01 + 1)

        int64 d11 * 100_000_000_000L
        + int64 d10 * 10_000_000_000L
        + int64 d09 * 1_000_000_000L
        + int64 d08 * 100_000_000L
        + int64 d07 * 10_000_000L
        + int64 d06 * 1_000_000L
        + int64 d05 * 100_000L
        + int64 d04 * 10_000L
        + int64 d03 * 1_000L
        + int64 d02 * 100L
        + int64 d01 * 10L
        + int64 d00 * 1L

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
                    test <@ PartTwo.largestJoltage (parseLine "987654321111111") = 987654321111L @>)

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
