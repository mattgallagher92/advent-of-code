module Day11

let parse (line: string) = line.Split ' ' |> Array.map int64

let successors stone =
    let engraving = string stone

    match engraving.Length % 2 = 0, stone with
    | _, 0L -> [| 1L |]
    | true, _ ->
        let firstHalf = engraving.Substring(0, engraving.Length / 2)
        let secondHalf = engraving.Substring(engraving.Length / 2)
        [| int firstHalf; int secondHalf |]
    | false, _ -> [| stone * 2024L |]

module PartOne =
    let iterate stones = stones |> Array.collect successors

    let iterateNTimes n =
        Array.init n (fun _ -> iterate) |> Array.reduce (>>)

    let solve (lines: string array) =
        lines[0] |> parse |> iterateNTimes 25 |> Array.length

module PartTwo =

    let cache = System.Collections.Generic.Dictionary<int * int64, int64>()

    let rec numberOfStonesAfterBlinks blinks startStone =

        cache.TryGet(blinks, startStone)
        |> Option.defaultWith (fun _ ->
            let f1 = numberOfStonesAfterBlinks (blinks - 1)
            let f3 = numberOfStonesAfterBlinks (blinks - 3)
            let f4 = numberOfStonesAfterBlinks (blinks - 4)
            let f5 = numberOfStonesAfterBlinks (blinks - 5)

            let result =
                match blinks, startStone with
                | 0, _ -> 1L
                | _, 0L -> f1 1
                | _, 1L when blinks > 2 -> [| 2; 0; 2; 4 |] |> Array.sumBy (int64 >> f3)
                | _, 2L when blinks > 2 -> [| 4; 0; 4; 8 |] |> Array.sumBy (int64 >> f3)
                | _, 3L when blinks > 2 -> [| 6; 0; 7; 2 |] |> Array.sumBy (int64 >> f3)
                | _, 4L when blinks > 2 -> [| 8; 0; 9; 6 |] |> Array.sumBy (int64 >> f3)
                | _, 5L when blinks > 4 -> [| 2; 0; 4; 8; 2; 8; 8; 0 |] |> Array.sumBy (int64 >> f5)
                | _, 6L when blinks > 4 -> [| 2; 4; 5; 7; 9; 4; 5; 6 |] |> Array.sumBy (int64 >> f5)
                | _, 7L when blinks > 4 -> [| 2; 8; 6; 7; 6; 0; 3; 2 |] |> Array.sumBy (int64 >> f5)
                | _, 8L when blinks > 4 -> [| 3; 2; 7; 7; 2; 6; 16192 |] |> Array.sumBy (int64 >> f5)
                | _, 9L when blinks > 4 -> [| 3; 6; 8; 6; 9; 1; 8; 4 |] |> Array.sumBy (int64 >> f5)
                | _, 16192L when blinks > 3 -> [| 3; 2; 7; 7; 2; 6; 16192 |] |> Array.sumBy (int64 >> f4)
                | _, _ -> startStone |> successors |> Array.sumBy f1

            cache[(blinks, startStone)] <- result

            result)

    let solveN n (lines: string array) =
        lines[0]
        |> parse
        |> Array.sumBy (fun stone -> numberOfStonesAfterBlinks n stone)

    let solve = solveN 75

module Test =

    open Expecto
    open Swensen.Unquote
    open FsCheck

    // Ensure stones are non-negative.
    type StoneGen() =
        static member Stone() : Arbitrary<int64> =
            Arb.Default.Int64() |> Arb.filter (fun s -> -1L < s)

    // Ensure oracle returns result in a reasonable time.
    type BlinksGen() =
        static member Blinks() : Arbitrary<int> =
            Arb.Default.Int32() |> Arb.filter (fun b -> 0 < b && b < 26)

    let all =
        testList "Day11" [
            let sampleInput = [| "125 17" |]

            testList "PartOne" [
                testCase "iterate works with sample input" (fun _ ->

                    let after1 = PartOne.iterate [| 125; 17 |]
                    let after2 = PartOne.iterate after1
                    let after3 = PartOne.iterate after2
                    let after4 = PartOne.iterate after3
                    let after5 = PartOne.iterate after4
                    let after6 = PartOne.iterate after5

                    let expectedAfter1 = [| 253000L; 1L; 7L |]
                    let expectedAfter2 = [| 253L; 0L; 2024L; 14168L |]
                    let expectedAfter3 = [| 512072L; 1L; 20L; 24L; 28676032L |]
                    let expectedAfter4 = [| 512L; 72L; 2024L; 2L; 0L; 2L; 4L; 2867L; 6032L |]
                    let expectedAfter5 = [| 1036288L; 7L; 2L; 20L; 24L; 4048L; 1L; 4048L; 8096L; 28L; 67L; 60L; 32L |]

                    let expectedAfter6 = [|
                        2097446912L
                        14168L
                        4048L
                        2L
                        0L
                        2L
                        4L
                        40L
                        48L
                        2024L
                        40L
                        48L
                        80L
                        96L
                        2L
                        8L
                        6L
                        7L
                        6L
                        0L
                        3L
                        2L
                    |]

                    test <@ after1 = expectedAfter1 @>
                    test <@ after2 = expectedAfter2 @>
                    test <@ after3 = expectedAfter3 @>
                    test <@ after4 = expectedAfter4 @>
                    test <@ after5 = expectedAfter5 @>
                    test <@ after6 = expectedAfter6 @>)


                testCase "solve works with sample input" (fun _ -> test <@ PartOne.solve sampleInput = 55312 @>)
            ]

            testList "PartTwo" [

                testPropertyWithConfig
                    {
                        FsCheckConfig.defaultConfig with
                            arbitrary = [ typeof<StoneGen>; typeof<BlinksGen> ]
                    }
                    "numberOfStonesAfterBlinks matches answer from PartOne"
                    (fun blinks stone ->
                        let oracle = Array.singleton >> PartOne.iterateNTimes blinks >> Array.length
                        test <@ PartTwo.numberOfStonesAfterBlinks blinks stone = oracle stone @>)

                testCase "solveN works with sample input" (fun _ ->
                    test <@ PartTwo.solveN 1 sampleInput = 3 @>
                    test <@ PartTwo.solveN 2 sampleInput = 4 @>
                    test <@ PartTwo.solveN 3 sampleInput = 5 @>
                    test <@ PartTwo.solveN 4 sampleInput = 9 @>
                    test <@ PartTwo.solveN 5 sampleInput = 13 @>
                    test <@ PartTwo.solveN 6 sampleInput = 22 @>
                    test <@ PartTwo.solveN 25 sampleInput = 55312 @>)
            ]
        ]

let dayFns = {
    Tests = Test.all
    UtilTests = []
    PartOne = PartOne.solve >> int64
    PartTwo = PartTwo.solve >> int64
}
