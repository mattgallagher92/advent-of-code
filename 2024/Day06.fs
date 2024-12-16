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

    /// The guard states split into sections where the guard is moving in one direction.
    let straights guardStates =
        guardStates
        |> Array.eachWithNext
        // Split when the current state's heading is different to the next state's heading.
        |> Seq.splitAfterMatches (fun (curr, next) ->
            match next with
            | Some { Heading = h } when h <> curr.Heading -> true
            | Some _
            | None -> false)
        |> Seq.map (Seq.map fst >> Seq.toArray)
        |> Seq.toArray

    let doIt map (guardStates: MappedAreaState array) =

        // TODO: pre-filter down to '#' locations?
        let rows = map |> Array2D.rows
        let cols = map |> Array2D.columns

        // TODO: just recurse from beginning rather than generating guard states first.
        (([], []), guardStates)
        ||> Array.fold (fun (collisionStates, potentialNewObstacleLocations) mapState ->
            let next = PartOne.nextState map (InMap mapState)

            // Check whether the current state is a collision location.
            let newCollisions =
                match next with
                | InMap next ->
                    if next.Heading <> mapState.Heading then
                        mapState :: collisionStates
                    else
                        collisionStates
                | LeftMap -> collisionStates

            // Check whether we can send back to a previous collision.
            let newPotentials =
                match mapState with
                | { Heading = Up; Position = r, c } ->
                    let row = rows[r]

                    let nextObstacleToRight =
                        row
                        |> Array.indexed
                        |> Array.tryFind (fun (i, ch) -> i > c && ch = '#')
                        |> Option.map fst

                    match nextObstacleToRight with
                    | Some nextOb ->
                        if
                            collisionStates
                            |> List.contains {
                                Position = r, nextOb - 1
                                Heading = Right
                            }
                        then
                            (r - 1, c) :: potentialNewObstacleLocations
                        else
                            potentialNewObstacleLocations
                    | None -> potentialNewObstacleLocations
                | { Heading = Right; Position = r, c } ->
                    let col = cols[c]

                    let nextObstacleDown =
                        col
                        |> Array.indexed
                        |> Array.tryFind (fun (i, ch) -> i > r && ch = '#')
                        |> Option.map fst

                    match nextObstacleDown with
                    | Some nextOb ->
                        if
                            collisionStates
                            |> List.contains {
                                Position = nextOb - 1, c
                                Heading = Down
                            }
                        then
                            (r, c + 1) :: potentialNewObstacleLocations
                        else
                            potentialNewObstacleLocations
                    | None -> potentialNewObstacleLocations
                | { Heading = Down; Position = r, c } ->
                    let row = rows[r]

                    let nextObstacleToLeft =
                        row
                        |> Array.indexed
                        |> Array.tryFindBack (fun (i, ch) -> i < c && ch = '#')
                        |> Option.map fst

                    match nextObstacleToLeft with
                    | Some nextOb ->
                        if
                            collisionStates
                            |> List.contains {
                                Position = r, nextOb + 1
                                Heading = Left
                            }
                        then
                            (r + 1, c) :: potentialNewObstacleLocations
                        else
                            potentialNewObstacleLocations
                    | None -> potentialNewObstacleLocations
                | { Heading = Left; Position = r, c } ->
                    let col = cols[c]

                    let nextObstacleUp =
                        col
                        |> Array.indexed
                        |> Array.tryFind (fun (i, ch) -> i < r && ch = '#')
                        |> Option.map fst

                    match nextObstacleUp with
                    | Some nextOb ->
                        if
                            collisionStates
                            |> List.contains {
                                Position = nextOb + 1, c
                                Heading = Up
                            }
                        then
                            (r, c - 1) :: potentialNewObstacleLocations
                        else
                            potentialNewObstacleLocations
                    | None -> potentialNewObstacleLocations

            newCollisions, newPotentials)
        |> snd

    // Answer is not 336.
    let solve (lines: string array) =
        let map = lines |> parse
        let guardStates = PartOne.allGuardStates map
        doIt map guardStates |> List.length

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
                testCase "straights works with sample map" (fun _ ->
                    let allGuardStates = PartOne.allGuardStates sampleMap

                    let result = PartTwo.straights allGuardStates

                    let expected = [|
                        [| 6..-1..1 |] |> Array.map (fun row -> { Position = row, 4; Heading = Up })
                        [| 4..+1..8 |] |> Array.map (fun col -> { Position = 1, col; Heading = Right })
                        [| 1..+1..6 |] |> Array.map (fun row -> { Position = row, 8; Heading = Down })
                        [| 8..-1..2 |] |> Array.map (fun col -> { Position = 6, col; Heading = Left })

                        [| 6..-1..4 |] |> Array.map (fun row -> { Position = row, 2; Heading = Up })
                        [| 2..+1..6 |] |> Array.map (fun col -> { Position = 4, col; Heading = Right })
                        [| 4..+1..8 |] |> Array.map (fun row -> { Position = row, 6; Heading = Down })
                        [| 6..-1..1 |] |> Array.map (fun col -> { Position = 8, col; Heading = Left })

                        [| 8..-1..7 |] |> Array.map (fun row -> { Position = row, 1; Heading = Up })
                        [| 1..+1..7 |] |> Array.map (fun col -> { Position = 7, col; Heading = Right })
                        [| 7..+1..9 |] |> Array.map (fun row -> { Position = row, 7; Heading = Down })
                    |]

                    test <@ result = expected @>)

                testCase "validObstacleLocations works with sample map" (fun _ ->
                    let result = PartTwo.doIt sampleMap (PartOne.allGuardStates sampleMap)
                    let expected = [| 6, 3; 7, 6; 7, 7; 8, 1; 8, 3; 9, 7 |]
                    test <@ Set result = Set expected @>)
            ]
        ]

let dayFns = {
    Tests = Test.all
    ReadInput = System.IO.File.ReadAllLines
    PartOne = PartOne.solve
    PartTwo = PartTwo.solve
}
