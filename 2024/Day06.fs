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

    /// The straight extended backwards until an obstacle is hit or the map is left.
    let extend map straight =
        let { Heading = d } as first = Array.head straight

        let prev state =
            let posBehind = MappedAreaState.posBehind state

            posBehind
            |> Array2D.tryGet map
            |> Option.bind (fun c ->
                match c with
                | '#' -> None
                | _ -> Some { Position = posBehind; Heading = d })

        let generator x = x |> Option.map (fun x -> x, prev x)
        // Go backwards one step at a time from first, then reverse the list so that it's as if it's going forwards.
        let extension = List.unfold generator (Some first) |> List.rev
        // Join the extension, which includes the first item of the straight to the rest of the straight.
        Array.append (extension |> List.toArray) (Array.tail straight)

    // TODO: This doesn't work; it sends onto another point on the path, but need to only send to already visited points. New plan:
    // Make a note of obstacles that have been hit so far.
    // Use that to calculate the places where turning back in that direction would cause the obstacle to be hit in the same way (along the straight).
    // Turn that into a list of states where a turn would cause a loop.
    // For every point along the path, check if it's one of those points.
    let validObstacleLocations map =
        let allGuardStates = map |> PartOne.allGuardStates
        let extendedStraights = allGuardStates |> straights |> Array.map (extend map)
        let initialPos = allGuardStates |> Array.head |> _.Position

        let posToWalkedStates = allGuardStates |> Array.groupBy _.Position |> dict

        let posToStraightStates =
            extendedStraights |> Array.collect id |> Array.groupBy _.Position |> dict

        // A valid location for an obstacle is just ahead of the places where the guard's regular path intersects one
        // of the extended straights and the guard is heading in the previous direction to the straight's direction,
        // so long as that location is not the guard's start position.
        allGuardStates
        |> Array.filter (fun x -> x.Position <> initialPos)
        |> Array.choose (fun walkedState ->
            let straightHeadings =
                posToStraightStates[walkedState.Position] |> Array.map _.Heading

            printfn $"%A{walkedState.Position} %A{walkedState.Heading}, straights: %A{straightHeadings}"

            if Array.contains (Direction.next walkedState.Heading) straightHeadings then
                Some(MappedAreaState.posAhead walkedState)
            else
                None)

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
                let moveUp col =
                    Array.map (fun row -> { Position = row, col; Heading = Up })

                let moveRight row =
                    Array.map (fun col -> { Position = row, col; Heading = Right })

                let moveDown col =
                    Array.map (fun row -> { Position = row, col; Heading = Down })

                let moveLeft row =
                    Array.map (fun col -> { Position = row, col; Heading = Left })

                testCase "straights works with sample map" (fun _ ->
                    let allGuardStates = PartOne.allGuardStates sampleMap

                    let result = PartTwo.straights allGuardStates

                    let expected = [|
                        [| 6..-1..1 |] |> moveUp 4
                        [| 4..+1..8 |] |> moveRight 1
                        [| 1..+1..6 |] |> moveDown 8
                        [| 8..-1..2 |] |> moveLeft 6

                        [| 6..-1..4 |] |> moveUp 2
                        [| 2..+1..6 |] |> moveRight 4
                        [| 4..+1..8 |] |> moveDown 6
                        [| 6..-1..1 |] |> moveLeft 8

                        [| 8..-1..7 |] |> moveUp 1
                        [| 1..+1..7 |] |> moveRight 7
                        [| 7..+1..9 |] |> moveDown 7
                    |]

                    test <@ result = expected @>)

                testCase "extend works with sample map and extension off map" (fun _ ->
                    let straight = [| 6..-1..1 |] |> moveUp 4
                    let result = PartTwo.extend sampleMap straight
                    let expected = [| 9..-1..1 |] |> moveUp 4
                    test <@ result = expected @>)

                testCase "extend works with sample map and extension into obstacle" (fun _ ->
                    let straight = [| 4..+1..9 |] |> moveRight 6
                    let result = PartTwo.extend sampleMap straight
                    let expected = [| 2..+1..9 |] |> moveRight 6
                    test <@ result = expected @>)

                testCase "validObstacleLocations works with sample map" (fun _ ->
                    let result = PartTwo.validObstacleLocations sampleMap
                    let expected = [| 6, 3; 7, 6; 7, 7; 8, 1; 8, 3; 9, 7 |]
                    test <@ Set result = Set expected @>)

            // testCase "solve works with sample input" (fun _ -> test <@ PartTwo.solve sampleInput = 6 @>)
            ]
        ]

let dayFns = {
    Tests = Test.all
    ReadInput = System.IO.File.ReadAllLines
    PartOne = PartOne.solve
    PartTwo = PartTwo.solve
}
