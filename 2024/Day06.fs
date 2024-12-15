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

    let posBehind { Position = row, col; Heading = d } =
        match d with
        | Up -> row + 1, col
        | Right -> row, col - 1
        | Down -> row - 1, col
        | Left -> row, col + 1

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

    open System.Collections.Generic

    // TODO: This doesn't work; it sends onto another point on the path, but need to only send to already visited points. New plan:
    let validObstacleLocations map =
        let rows = map |> Array2D.rows
        let cols = map |> Array2D.columns
        let initialPos = map |> Array2D.findIndex ((=) '^')

        // TODO: could replace with function state -> bool???
        let statesThatWouldCauseLoop = HashSet()
        let viableObstacleLocations = HashSet()

        let mutable guardState = InMap { Position = initialPos; Heading = Up }

        let isInMap =
            function
            | InMap _ -> true
            | LeftMap -> false

        while isInMap guardState do
            guardState <-
                match guardState with
                | InMap state ->
                    let posAhead = MappedAreaState.posAhead state

                    if Array2D.isWithinBounds map posAhead then
                        if posAhead ||> Array2D.get map = '#' then
                            match state.Heading with
                            | Up ->
                                let currentRowIx, currentColIx = state.Position
                                let currentCol = cols[currentColIx]
                                // TODO: cache obstacle locations for quick lookup.
                                let nextObstacleDownRowIx =
                                    currentCol
                                    |> Array.indexed
                                    |> Array.tryFind (fun (i, c) -> i > currentRowIx && c = '#')
                                    |> Option.map fst

                                let maxRowIx = nextObstacleDownRowIx |> Option.defaultValue (currentCol.Length - 1)

                                for r in currentRowIx..maxRowIx do
                                    statesThatWouldCauseLoop.Add {
                                        Position = r, currentColIx
                                        Heading = Left
                                    }
                                    |> ignore
                            | Right -> TODO
                            | Down -> TODO
                            | Left -> TODO

                            InMap {
                                state with
                                    Heading = Direction.next state.Heading
                            }
                        else
                            InMap { state with Position = posAhead }
                    else
                        LeftMap
                | LeftMap -> LeftMap

        [||]
    // Make a note of obstacles that have been hit so far.
    // Use that to calculate the places where turning back in that direction would cause the obstacle to be hit in the same way (along the straight).
    // Turn that into a list of states where a turn would cause a loop.
    // For every point along the path, check if it's one of those points.

    let solve lines =
        lines |> parse |> validObstacleLocations |> Array.distinct |> Array.length

module Test =

    open Expecto
    open Swensen.Unquote

    let all =
        testList "Day6" [
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

            let sampleMap = parse sampleInput

            testList "PartOne" [
                testCase "solve works with sample input" (fun _ -> test <@ PartOne.solve sampleInput = 41 @>)
            ]

            testList "PartTwo" [
            // testCase "validObstacleLocations works with sample map" (fun _ ->
            //     let result = PartTwo.validObstacleLocations sampleMap
            //     let expected = [| 6, 3; 7, 6; 7, 7; 8, 1; 8, 3; 9, 7 |]
            //     test <@ Set result = Set expected @>)

            // testCase "solve works with sample input" (fun _ -> test <@ PartTwo.solve sampleInput = 6 @>)
            ]
        ]

let dayFns = {
    Tests = Test.all
    ReadInput = System.IO.File.ReadAllLines
    PartOne = PartOne.solve
    PartTwo = PartTwo.solve
}
