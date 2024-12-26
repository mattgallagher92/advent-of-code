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

    open System.Collections.Generic

    [<Literal>]
    let maxBlinks = 75

    /// Stone to list of arrays produced at each successive iteration, in reverse order.
    let cache = Dictionary<int64, int64 array list>()

    // TODO: add explanatory comments.
    let rec stonesAfterBlinks n startStone =
        match n, cache.TryGet startStone with
        | 1, _ -> [ successors startStone ]
        | _, Some result when result.Length >= n ->
            // Want last n elements.
            result |> List.skip (result.Length - n)
        | _, Some result ->
            let iterationsCached = result.Length

            // TODO: this is still very slow when n is large because result.Head contains a lot of stones; it's growing as roughly 1.5 ^ n.
            let lists = result.Head |> Array.map (stonesAfterBlinks (n - iterationsCached))

            let newResult = [
                // lists have length n - iterationsCached because they're produced by stonesAfterBlinks (n - iterationsCached).
                for i in 1 .. (n - iterationsCached) do
                    lists |> Array.map (fun l -> l[i - 1]) |> Array.reduce Array.append
                yield! result
            ]

            cache[startStone] <- newResult
            newResult
        | _, None ->
            let lists = startStone |> successors |> Array.map (stonesAfterBlinks (n - 1))

            let result = [
                for i in 1 .. n - 1 do
                    lists |> Array.map (fun l -> l[i - 1]) |> Array.reduce Array.append
                successors startStone
            ]

            cache[startStone] <- result
            result

    let solveN n (lines: string array) =
        lines[0]
        |> parse
        |> Array.collect (stonesAfterBlinks n >> List.head)
        |> Array.length

    let solve = solveN 75

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

                testCase "stoneAfterBlinks works with sample input" (fun _ ->

                    let after n =
                        [| 125L; 17L |]
                        |> Array.map (PartTwo.stonesAfterBlinks 6)
                        |> Array.collect (List.item (6 - n))

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

                    test <@ after 1 = expectedAfter1 @>
                    test <@ after 2 = expectedAfter2 @>
                    test <@ after 3 = expectedAfter3 @>
                    test <@ after 4 = expectedAfter4 @>
                    test <@ after 5 = expectedAfter5 @>
                    test <@ after 6 = expectedAfter6 @>)

                testCase "solveN works with sample input" (fun _ -> test <@ PartTwo.solveN 25 sampleInput = 55312 @>)
            ]
        ]

let dayFns = {
    Tests = Test.all
    UtilTests = []
    PartOne = PartOne.solve >> int64
    PartTwo = PartTwo.solve >> int64
}
