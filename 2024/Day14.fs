module Day14

let regexMatch pattern input =
    System.Text.RegularExpressions.Regex.Match(input, pattern)

let modulo n a =
    a % n |> fun m -> if m < 0 then m + n else m

type Robot = { Pos0: int * int; Velocity: int * int }

let parseLine (line: string) =
    line
    |> regexMatch "p=(\d+),(\d+) v=([-\d]+),([-\d]+)"
    |> _.Groups
    |> fun gs -> {
        Pos0 = int gs[1].Value, int gs[2].Value
        Velocity = int gs[3].Value, int gs[4].Value
    }

module PartOne =

    let positionAfterNSteps width height n { Pos0 = x0, y0; Velocity = vx, vy } =
        let xN = x0 + vx * n |> modulo width
        let yN = y0 + vy * n |> modulo height
        xN, yN

    let safetyFactor width height positions =
        let w = width / 2
        let h = height / 2

        positions
        |> Array.filter (fun (x, y) -> x <> w && y <> h)
        |> Array.groupBy (fun (x, y) -> x < w, y < h)
        |> Array.map (snd >> Array.length)
        |> Array.reduce (*)

    let solveDim width height lines =
        lines
        |> Array.map (parseLine >> positionAfterNSteps width height 100)
        |> safetyFactor width height

    let solve = solveDim 101 103

module PartTwo =

    let solve (lines: string array) = -1

module Test =

    open Expecto
    open Swensen.Unquote

    let all =
        testList "Day14" [
            let sampleInput = [|
                "p=0,4 v=3,-3"
                "p=6,3 v=-1,-3"
                "p=10,3 v=-1,2"
                "p=2,0 v=2,-1"
                "p=0,0 v=1,3"
                "p=3,0 v=-2,-2"
                "p=7,6 v=-1,-3"
                "p=3,0 v=-1,-2"
                "p=9,3 v=2,3"
                "p=7,3 v=-1,2"
                "p=2,4 v=2,-3"
                "p=9,5 v=-3,-3"
            |]

            testList "PartOne" [
                testCase "" (fun _ ->
                    let robot = parseLine "p=2,4 v=2,-3"

                    let positions =
                        [| 0..5 |] |> Array.map (fun n -> PartOne.positionAfterNSteps 11 7 n robot)

                    test <@ positions = [| 2, 4; 4, 1; 6, 5; 8, 2; 10, 6; 1, 3 |] @>)

                testCase "solveDim works with sample input" (fun _ -> test <@ PartOne.solveDim 11 7 sampleInput = 12 @>)
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
