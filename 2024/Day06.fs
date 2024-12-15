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

module MappedAreaState =

    let posAhead { Position = row, col; Heading = d } =
        match d with
        | Up -> row - 1, col
        | Right -> row, col + 1
        | Down -> row + 1, col
        | Left -> row, col - 1

type GuardState =
    | InMap of MappedAreaState
    | LeftMap

let parse (lines: string array) =
    lines |> Array.map Seq.toArray |> array2D

module PartOne =

    let nextState (map: char array2d) guardState =
        match guardState with
        | LeftMap -> LeftMap
        | InMap state ->
            let posAhead = MappedAreaState.posAhead state

            if Array2D.isWithinBounds map posAhead then
                if posAhead ||> Array2D.get map = '#' then
                    InMap {
                        state with
                            Heading = Direction.next state.Heading
                    }
                else
                    InMap { state with Position = posAhead }
            else
                LeftMap

    let allGuardStates map =
        let generator state =
            match state with
            | InMap x -> Some(x, nextState map state)
            | LeftMap -> None

        InMap {
            Position = map |> Array2D.findIndex ((=) '^')
            Heading = Up
        }
        |> List.unfold generator
        |> List.toArray

    let solve lines =
        lines
        |> parse
        |> allGuardStates
        |> Array.map _.Position
        |> Array.distinct
        |> Array.length

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
