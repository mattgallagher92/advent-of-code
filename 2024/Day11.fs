module Day11

let parse (line: string) = line.Split ' ' |> Array.map int64

module PartOne =

    let successors stone =
        let engraving = string stone

        match engraving.Length % 2 = 0, stone with
        | _, 0L -> [| 1L |]
        | true, _ ->
            let firstHalf = engraving.Substring(0, engraving.Length / 2)
            let secondHalf = engraving.Substring(engraving.Length / 2)
            [| int firstHalf; int secondHalf |]
        | false, _ -> [| stone * 2024L |]

    let iterate stones = stones |> Array.collect successors

    let iterateNTimes n =
        Array.init n (fun _ -> iterate) |> Array.reduce (>>)

    let solve (lines: string array) =
        lines[0] |> parse |> iterateNTimes 25 |> Array.length

module PartTwo =

    let solve (lines: string array) = -1

module Test =

    open Expecto
    open Swensen.Unquote

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
                testCase "solve works with sample input" (fun _ -> test <@ PartTwo.solve sampleInput = -1 @>)
            ]
        ]

let dayFns = {
    Tests = Test.all
    UtilTests = []
    PartOne = PartOne.solve >> int64
    PartTwo = PartTwo.solve >> int64
}
