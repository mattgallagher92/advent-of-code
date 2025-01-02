module Day14

let regexMatch pattern input =
    System.Text.RegularExpressions.Regex.Match(input, pattern)

let modulo n a =
    a % n |> fun m -> if m < 0 then m + n else m

type Robot = { Pos: int * int; Velocity: int * int }

let parseLine (line: string) =
    line
    |> regexMatch "p=(\d+),(\d+) v=([-\d]+),([-\d]+)"
    |> _.Groups
    |> fun gs -> {
        Pos = int gs[1].Value, int gs[2].Value
        Velocity = int gs[3].Value, int gs[4].Value
    }

type PerQuadrantCount = { TL: int; TR: int; BL: int; BR: int }

let robotsPerQuadrant midX midY positions =
    let grouped =
        positions
        |> Array.filter (fun (x, y) -> x <> midX && y <> midY)
        |> Array.groupBy (fun (x, y) -> x < midX, y < midY)

    {
        TL =
            grouped
            |> Array.tryFind (fst >> (=) (true, true))
            |> Option.map (snd >> Array.length)
            |> Option.defaultValue 0
        TR =
            grouped
            |> Array.tryFind (fst >> (=) (false, true))
            |> Option.map (snd >> Array.length)
            |> Option.defaultValue 0
        BL =
            grouped
            |> Array.tryFind (fst >> (=) (true, false))
            |> Option.map (snd >> Array.length)
            |> Option.defaultValue 0
        BR =
            grouped
            |> Array.tryFind (fst >> (=) (false, false))
            |> Option.map (snd >> Array.length)
            |> Option.defaultValue 0
    }

module PartOne =

    let positionAfterNSteps width height n { Pos = x0, y0; Velocity = vx, vy } =
        let xN = x0 + vx * n |> modulo width
        let yN = y0 + vy * n |> modulo height
        xN, yN

    let safetyFactor pqc = pqc.TL * pqc.TR * pqc.BL * pqc.BR

    let solveDim width height lines =
        lines
        |> Array.map (parseLine >> positionAfterNSteps width height 100)
        |> robotsPerQuadrant (width / 2) (height / 2)
        |> safetyFactor

    let solve = solveDim 101 103

module PartTwo =

    let nextPosition width height { Pos = x, y; Velocity = vx, vy } =
        let x' = x + vx |> modulo width
        let y' = y + vy |> modulo height
        x', y'

    let areBalanced midX midY robots =
        robots
        |> Array.map _.Pos
        |> robotsPerQuadrant midX midY
        |> fun c -> c.TL = c.TR && c.BL = c.BR

    let solveDim width height (lines: string array) =
        let midX, midY = width / 2, height / 2

        let areBalanced = areBalanced midX midY
        let next = Array.map (fun r -> { r with Pos = nextPosition width height r })

        let mutable robots = lines |> Array.map parseLine

        let printRobots () =
            for y in 0..height do
                for x in 0..width do
                    if robots |> Array.exists (fun r -> r.Pos = (x, y)) then
                        printf "x"
                    else
                        printf " "

                printfn ""

            printfn ""

        let mutable elapsedSeconds = 0
        let mutable isEasterEgg = false

        while not isEasterEgg do

            while not (areBalanced robots) do
                robots <- robots |> next
                elapsedSeconds <- elapsedSeconds + 1

            printfn $"Are the robots arranged in a Christmas tree after %i{elapsedSeconds} seconds?"
            printRobots ()
            printfn "Y/N"

            isEasterEgg <- System.Console.ReadLine() = "Y"

            robots <- robots |> next
            elapsedSeconds <- elapsedSeconds + 1

        printfn $"First in a Christmas tree formation after %i{elapsedSeconds} seconds."
        elapsedSeconds

    let solve lines = solveDim 101 103 lines

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

            testList "PartTwo" []
        ]

let dayFns = {
    Tests = Test.all
    UtilTests = []
    PartOne = PartOne.solve >> int64
    PartTwo = PartTwo.solve >> int64
}
