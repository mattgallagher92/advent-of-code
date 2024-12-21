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
    let map = lines |> Array.map Seq.toArray |> array2D
    let startPos = map |> Array2D.findIndex ((=) '^')
    map, startPos

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

/// The (possibly infinite) sequence of MappedAreaStates that the guard is in.
let guardStates map startPos =
    let generator state =
        match state with
        | InMap x -> Some(x, nextState map state)
        | LeftMap -> None

    InMap { Position = startPos; Heading = Up } |> Seq.unfold generator

let guardPositions map startPos =
    (map, startPos) ||> guardStates |> Seq.map _.Position

/// The enumerated sequence of distinct positions the guard visits.
let allGuardPositions map startPos =
    (map, startPos) ||> guardPositions |> Seq.toArray |> Array.distinct

module PartOne =

    let solve lines =
        lines |> parse ||> allGuardPositions |> Array.length

module PartTwo =

    open System.Collections.Generic

    let guardGetsCaughtInLoop startPos map =
        (map, startPos)
        ||> guardStates
        |> fun ss -> (false, HashSet<_>()), ss
        ||> Seq.scan (fun (hasRepeated, visited) current ->
            let hasRepeated = hasRepeated || visited.Contains current
            visited.Add current |> ignore
            hasRepeated, visited)
        |> Seq.map fst
        // Because the map is finite, the sequence either starts repeating or terminates in a finite number of steps.
        |> Seq.tryFind id
        |> Option.defaultValue false

    let positionsWhereNewObstructionsCauseLoops map startPos =
        let positionsToTest =
            (map, startPos) ||> allGuardPositions |> Array.except [| startPos |]

        let mapsToTest =
            positionsToTest
            |> Array.map (fun (r, c) -> (r, c), map |> Array2D.updateAt r c '#')

        mapsToTest
        |> Array.mapSnd (guardGetsCaughtInLoop startPos)
        |> Array.filter snd
        |> Array.map fst

    // TODO: takes about 5 seconds to solve with my puzzle input. Is there a way to be smarter?
    let solve (lines: string array) =
        parse lines ||> positionsWhereNewObstructionsCauseLoops |> Array.length

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

            let parsedSample = parse sampleInput

            testList "PartOne" [
                testCase "solve works with sample input" (fun _ -> test <@ PartOne.solve sampleInput = 41 @>)
            ]

            testList "PartTwo" [
                testCase "positionsWhereNewObstructionsCauseLoops works with sample input" (fun _ ->
                    let result = parsedSample ||> PartTwo.positionsWhereNewObstructionsCauseLoops |> set
                    let expected = set [| 6, 3; 7, 6; 7, 7; 8, 1; 8, 3; 9, 7 |]
                    test <@ result = expected @>)

                testCase "solve works with sample input" (fun _ -> test <@ PartTwo.solve sampleInput = 6 @>)
            ]
        ]

let dayFns = {
    Tests = Test.all
    ReadInput = System.IO.File.ReadAllLines
    PartOne = PartOne.solve
    PartTwo = PartTwo.solve
}
