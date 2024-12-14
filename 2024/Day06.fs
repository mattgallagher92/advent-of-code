module Day6

type Direction =
    | Up
    | Right
    | Down
    | Left

module Direction =

    let next =
        function
        | Up -> Right
        | Right -> Down
        | Down -> Left
        | Left -> Up

type MappedAreaState = {
    Position: int * int
    Heading: Direction
}

type GuardState =
    | InMap of MappedAreaState
    | LeftMap

module PartOne =

    let nextState (map: char array2d) guardState =
        match guardState with
        | LeftMap -> LeftMap
        | InMap { Position = row, col; Heading = d } ->
            let posAhead =
                match d with
                | Up -> row - 1, col
                | Right -> row, col + 1
                | Down -> row + 1, col
                | Left -> row, col - 1

            if Array2D.isWithinBounds map posAhead then
                if posAhead ||> Array2D.get map = '#' then
                    InMap {
                        Position = row, col
                        Heading = Direction.next d
                    }
                else
                    InMap { Position = posAhead; Heading = d }
            else
                LeftMap

    let solve (lines: string array) =
        let map = lines |> Array.map Seq.toArray |> array2D
        let initialPos = map |> Array2D.findIndex ((=) '^')

        let generator state =
            match nextState map state with
            | InMap x -> Some(x.Position, InMap x)
            | LeftMap -> None

        InMap { Position = initialPos; Heading = Up }
        |> List.unfold generator
        |> List.distinct
        |> List.length

module PartTwo =

    let solve (lines: string array) = -1

module Test =

    open Expecto
    open Swensen.Unquote

    let all =
        testList "Day6" [
            testList "PartOne" [
                let sampleInput = [|
                    "....#....."
                    ".........#"
                    ".........."
                    "..#......."
                    ".......#.."
                    ".........."
                    ".#..^....."
                    "........#."
                    "#........."
                    "......#..."
                |]

                testCase "solve works with sample input" (fun _ -> test <@ PartOne.solve sampleInput = 41 @>)
            ]
            testList "PartTwo" [

            ]
        ]

let dayFns = {
    Tests = Test.all
    ReadInput = System.IO.File.ReadAllLines
    PartOne = PartOne.solve
    PartTwo = PartTwo.solve
}
