module Day18

module PartOne =

    let solve' maxCoordinate elapsedNanos (lines: string array) =
        let corrupted =
            lines
            |> Array.take elapsedNanos
            |> Array.map (
                Regex.regexMatch "(\d+),(\d+)"
                >> fun m -> int m.Groups[1].Value, int m.Groups[2].Value
            )

        let uncorrupted =
            [|
                for x in 0..maxCoordinate do
                    for y in 0..maxCoordinate do
                        x, y
            |]
            |> Array.except corrupted
            |> set

        let neighbours (x, y) =
            set [ x - 1, y; x, y - 1; x, y + 1; x + 1, y ]
            |> Set.filter uncorrupted.Contains

        let start = 0, 0
        let exit = maxCoordinate, maxCoordinate

        (set [ exit ], set [ exit ])
        |> Array.unfold (fun (alreadyVisited, coordinatesAtPreviousDistance) ->
            if Set.contains start coordinatesAtPreviousDistance then
                None
            else
                let ns = coordinatesAtPreviousDistance |> Set.map neighbours |> Set.unionMany
                let newCoords = Set.difference ns alreadyVisited
                let visited = Set.union alreadyVisited newCoords
                Some(newCoords, (visited, newCoords)))
        |> Array.length

    let solve (lines: string array) = solve' 70 1024 lines

module PartTwo =

    let solve (lines: string array) = -1

module Test =

    open Expecto
    open Swensen.Unquote

    let all =
        testList "Day18" [
            let sampleInput = [|
                "5,4"
                "4,2"
                "4,5"
                "3,0"
                "2,1"
                "6,3"
                "2,4"
                "1,5"
                "0,6"
                "3,3"
                "2,6"
                "5,1"
                "1,2"
                "5,5"
                "2,5"
                "6,5"
                "1,4"
                "0,4"
                "6,4"
                "1,1"
                "6,1"
                "1,0"
                "0,5"
                "1,6"
                "2,0"
            |]

            testList "PartOne" [
                testCase "solve works with sample input" (fun _ -> test <@ PartOne.solve' 6 12 sampleInput = 22 @>)
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
