module Day10

let parse (lines: string array) =
    lines |> Array.map (Seq.map (_.ToString() >> int) >> Seq.toArray) |> array2D

let nLocations n map = map |> Array2D.findAllIndexes ((=) n)

module PartOne =

    open System.Collections.Generic

    [<TailCall>]
    let rec private inner map n (nPlusOneLocationsToAccessibleNines: IDictionary<int * int, Set<int * int>>) =
        if n = -1 then
            nPlusOneLocationsToAccessibleNines
        else
            map
            |> nLocations n
            |> Array.map (fun nLocation ->
                nLocation
                |> Array2D.adjacentIndexes map
                |> Array.collect (nPlusOneLocationsToAccessibleNines.TryGet >> Option.toArray)
                |> Set.unionMany
                |> fun accessibleNines -> nLocation, accessibleNines)
            |> dict
            |> inner map (n - 1)

    let trailHeadsToAccessibleNines map =
        map |> nLocations 9 |> Array.map (fun l -> l, set [ l ]) |> dict |> inner map 8

    let solve (lines: string array) =
        lines
        |> parse
        |> trailHeadsToAccessibleNines
        |> Seq.sumBy (_.Value >> Set.count)

module PartTwo =

    let solve (lines: string array) = -1

module Test =

    open Expecto
    open Swensen.Unquote

    let all =
        testList "Day10" [
            let sampleInput = [|
                "89010123"
                "78121874"
                "87430965"
                "96549874"
                "45678903"
                "32019012"
                "01329801"
                "10456732"
            |]

            testCase "nLocations works with sample input" (fun _ ->
                let expected = [| 0, 1; 2, 5; 3, 0; 3, 4; 4, 5; 5, 4; 6, 4 |]
                test <@ nLocations 9 (parse sampleInput) = expected @>)

            testList "PartOne" [
                testCase "solve works with sample input" (fun _ -> test <@ PartOne.solve sampleInput = 36 @>)
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
