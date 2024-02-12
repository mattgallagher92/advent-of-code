module Day23

let startPosition lines = lines |> Array.head |> Seq.findIndex ((=) '.') |> fun col -> 0, col
let endPosition lines = lines |> Array.last |> Seq.findIndex ((=) '.') |> fun col -> lines.Length - 1, col

let above map (row, col) = if row = 0 then None else Array2D.get map (row - 1) col |> Some
let below map (row, col) = if row = Array2D.length1 map - 1 then None else Array2D.get map (row + 1) col |> Some
let toLeftOf map (row, col) = if col = 0 then None else Array2D.get map row (col - 1) |> Some
let toRightOf map (row, col) = if col = Array2D.length2 map - 1 then None else Array2D.get map row (col + 1) |> Some

let isAbove (r1, c1) (r2, c2) = r2 = r1 - 1 && c2 = c1
let isBelow (r1, c1) (r2, c2) = r2 = r1 + 1 && c2 = c1
let isToLeftOf (r1, c1) (r2, c2) = r2 = r1 && c2 = c1 - 1
let isToRightOf (r1, c1) (r2, c2) = r2 = r1 && c2 = c1 + 1

module PartOne =

    type InProgressState = {
        StepsSoFar: int
        Previous: (int * int) option
        Current: int * int
    }

    type Hike =
        | InProgress of InProgressState
        | Finished of steps:int

    let nextPositions map previous (row, col) = [|

        if previous |> Option.map (not << isAbove (row, col)) |> Option.defaultValue true then
            match above map (row, col) with
            | Some '.' | Some '^' | Some '<' | Some '>' -> row - 1, col
            | Some 'v' | Some '#' | None -> ()
            | Some _ -> failwith "Invalid char"

        if previous |> Option.map (not << isToLeftOf (row, col)) |> Option.defaultValue true then
            match toLeftOf map (row, col) with
            | Some '.' | Some '^' | Some 'v' | Some '<' -> row, col - 1
            | Some '>' | Some '#' | None -> ()
            | Some _ -> failwith "Invalid char"

        if previous |> Option.map (not << isToRightOf (row, col)) |> Option.defaultValue true then
            match toRightOf map (row, col) with
            | Some '.' | Some '^' | Some 'v' | Some '>' -> row, col + 1
            | Some '<' | Some '#' | None -> ()
            | Some _ -> failwith "Invalid char"

        if previous |> Option.map (not << isBelow (row, col)) |> Option.defaultValue true then
            match below map (row, col) with
            | Some '.' | Some 'v' | Some '<' | Some '>' -> row + 1, col
            | Some '^' | Some '#' | None -> ()
            | Some _ -> failwith "Invalid char"

    |]

    /// Assumes no loops.
    let hikeLengths lines =

        let startPos = startPosition lines
        let endPos = endPosition lines
        let map = lines |> array2D

        let rec inner hikes =

            let nextHikes =
                hikes
                |> Array.collect
                    (function
                    | Finished steps ->
                        [| Finished steps |]
                    | InProgress { StepsSoFar = steps; Previous = prev; Current = curr } ->
                        nextPositions map prev curr
                        |> Array.map (fun next ->
                            if next = endPos then
                                Finished (steps + 1)
                            else
                                InProgress { StepsSoFar = steps + 1; Previous = Some curr; Current = next }))

            if nextHikes |> Array.exists (function InProgress _ -> true | Finished _ -> false) then
                inner nextHikes
            else
                nextHikes |> Array.map (function InProgress _ -> failwith "bug" | Finished steps -> steps)

        inner [| InProgress { StepsSoFar = 0; Previous = None; Current = startPos } |]

    let solve lines = lines |> hikeLengths |> Array.max

module PartTwo =

    type InProgressState = {
        StepsSoFar: int
        Visited: (int * int) Set
        Current: int * int
    }

    type Hike =
        | InProgress of InProgressState
        | Finished of steps:int

    let adjacentPositions map (row, col) = [|

        match above map (row, col) with
        | Some '.' | Some 'v' | Some '^' | Some '<' | Some '>' -> row - 1, col
        | Some '#' | None -> ()
        | Some _ -> failwith "Invalid char"

        match toLeftOf map (row, col) with
        | Some '.' | Some '^' | Some 'v' | Some '<' | Some '>' -> row, col - 1
        | Some '#' | None -> ()
        | Some _ -> failwith "Invalid char"

        match toRightOf map (row, col) with
        | Some '.' | Some '^' | Some 'v' | Some '<' | Some '>' -> row, col + 1
        | Some '#' | None -> ()
        | Some _ -> failwith "Invalid char"

        match below map (row, col) with
        | Some '.' | Some '^' | Some 'v' | Some '<' | Some '>' -> row + 1, col
        | Some '#' | None -> ()
        | Some _ -> failwith "Invalid char"

    |]

    /// Finds the positions corresponding to places where a decision about which direction to follow has to be made.
    let findBranchPoints map = [|
        for row in 0 .. Array2D.length1 map - 1 do
            for col in 0 .. Array2D.length2 map - 1 do
                if Array2D.get map row col <> '#' && Array.length (adjacentPositions map (row, col)) > 2 then
                     row, col
    |]

    /// A node is either the start position, end position or a branch point.
    let calculateDistancesBetweenNodes startPosition endPosition map =

        let nodes = [| yield startPosition; yield! findBranchPoints map; yield endPosition |]
        let nodeSet = Set nodes

        nodes
        |> Array.collect (fun node ->
            adjacentPositions map node
            |> Array.map (fun adjacent ->

                let pathFromAdjacentToNextNode =
                    (node, adjacent)
                    |> Array.unfold (fun (prev, current) ->
                        if nodeSet.Contains current then
                            None
                        else
                            let next =
                                match adjacentPositions map current |> Array.except [| prev |] with
                                | [| n |] -> n
                                | a -> failwith $"Should have one next pos for %A{prev}->%A{current}, but have %A{a}."
                            Some (next, (current, next)))

                let nextNode = pathFromAdjacentToNextNode |> Array.last
                (node, nextNode), pathFromAdjacentToNextNode.Length + 1))

    let hikeLengths startPos endPos distancesBetweenNodes =

        // Given a node, returns the adjacent nodes with the distance to those nodes.
        let adjacentNodeLookup =
            distancesBetweenNodes
            |> Array.groupBy (fst >> fst)
            |> Array.map (fun (n, group) -> n, group |> Array.map (fun ((_, n'), d) -> n', d))
            |> dict

        let rec inner hikes =

            let nextHikes =
                hikes
                |> Array.collect
                    (function

                    | Finished steps ->
                        [| Finished steps |]

                    | InProgress { StepsSoFar = steps; Visited = visited; Current = curr } ->
                        adjacentNodeLookup[curr]
                        |> Array.filter (fun (n, _) -> not (visited.Contains n))
                        |> Array.map (fun (next, distance) ->
                            if next = endPos then
                                Finished (steps + distance)
                            else
                                InProgress {
                                    StepsSoFar = steps + distance
                                    Visited = visited |> Set.add next
                                    Current = next
                                }))

            if nextHikes |> Array.exists (function InProgress _ -> true | Finished _ -> false) then
                inner nextHikes
            else
                nextHikes |> Array.map (function InProgress _ -> failwith "bug" | Finished steps -> steps)

        inner [| InProgress { StepsSoFar = 0; Visited = Set [| startPos |]; Current = startPos } |]

    let solve lines =

        let startPos = startPosition lines
        let endPos = endPosition lines

        lines
        |> array2D
        |> calculateDistancesBetweenNodes startPos endPos
        |> hikeLengths startPos endPos
        |> Array.max

module Test =

    open Expecto

    let sampleLines = [|
        "#.#####################"
        "#.......#########...###"
        "#######.#########.#.###"
        "###.....#.>.>.###.#.###"
        "###v#####.#v#.###.#.###"
        "###.>...#.#.#.....#...#"
        "###v###.#.#.#########.#"
        "###...#.#.#.......#...#"
        "#####.#.#.#######.#.###"
        "#.....#.#.#.......#...#"
        "#.#####.#.#.#########v#"
        "#.#...#...#...###...>.#"
        "#.#.#v#######v###.###v#"
        "#...#.>.#...>.>.#.###.#"
        "#####v#.#.###v#.#.###.#"
        "#.....#...#...#.#.#...#"
        "#.#########.###.#.#.###"
        "#...###...#...#...#.###"
        "###.###.#.###v#####v###"
        "#...#...#.#.>.>.#.>.###"
        "#.###.###.#.###.#.#v###"
        "#.....###...###...#...#"
        "#####################.#"
    |]

    let sampleInput = testList "sample input" [

        let startPos = (0, 1)
        let endPos = (22, 21)
        let sampleMap = sampleLines |> array2D

        test "start position is (0, 1)" {
            let actual = startPosition sampleLines
            Expect.equal actual startPos ""
        }

        test "end position is (22, 21)" {
            let actual = endPosition sampleLines
            Expect.equal actual actual ""
        }

        testList "around (3, 11)" [

            test "above" {
                let x = above sampleMap (3, 11)
                Expect.equal x (Some '#') ""
            }

            test "below" {
                let x = below sampleMap (3, 11)
                Expect.equal x (Some 'v') ""
            }

            test "toLeft" {
                let x = toLeftOf sampleMap (3, 11)
                Expect.equal x (Some '>') ""
            }

            test "toRight" {
                let x = toRightOf sampleMap (3, 11)
                Expect.equal x (Some '>') ""
            }
        ]

        testList "PartOne.nextPositions" [

            test "correct when no previous" {
                let next = (None, startPos) ||> PartOne.nextPositions sampleMap
                Expect.equal next [| (1, 1) |] ""
            }

            test "correct when on a straight" {
                let next = (Some (0, 1), (1, 1)) ||> PartOne.nextPositions sampleMap
                Expect.equal next [| (1, 2) |] ""
            }

            test "correct when at a branch point" {
                let next = (Some (4, 3), (5, 3)) ||> PartOne.nextPositions sampleMap
                Expect.equal next [| (5, 4); (6, 3) |] ""
            }

        ]

        test "PartOne.hikeLengths gives correct values" {
            let lengths = PartOne.hikeLengths sampleLines |> Array.sort
            Expect.equal lengths [| 74; 82; 82; 86; 90; 94 |] ""
        }

        test "PartTwo.findBranchPoints returns correct values" {
            let branchPoints = sampleMap |> PartTwo.findBranchPoints
            Expect.equal branchPoints [| (3, 11); (5, 3); (11, 21); (13, 5); (13, 13); (19, 13); (19, 19) |] ""
        }

        test "PartTwo.calculateDistancesBetweenNodes contains correct values" {
            let distances = PartTwo.calculateDistancesBetweenNodes startPos endPos sampleMap
            let expected = [|
                ((0, 1), (5, 3)), 15
                ((5, 3), (3, 11)), 22
                ((13, 5), (13, 13)), 12
                ((19, 19), (22, 21)), 5
            |]
            Expect.containsAll distances expected ""
        }


        test "PartTwo.hikeLengths has correct maximum" {
            let lengths =
                PartTwo.calculateDistancesBetweenNodes startPos endPos sampleMap
                |> PartTwo.hikeLengths startPos endPos
                |> Array.max
            Expect.equal lengths 154 ""
        }

    ]

    let all = testList "Day 23" [
        sampleInput
    ]