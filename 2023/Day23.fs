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

module PartOne =

    type InProgressState = {
        StepsSoFar: int
        Previous: (int * int) option
        Current: int * int
    }

    type Hike =
        | InProgress of InProgressState
        | Finished of steps:int

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

        let sampleMap = sampleLines |> array2D

        test "start position is (0, 1)" {
            let startPos = startPosition sampleLines
            Expect.equal startPos (0, 1) ""
        }

        test "end position is (22, 21)" {
            let endPos = endPosition sampleLines
            Expect.equal endPos (22, 21) ""
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

        testList "nextPositions" [

            test "correct when no previous" {
                let next = (None, (0, 1)) ||> nextPositions sampleMap
                Expect.equal next [| (1, 1) |] ""
            }

            test "correct when on a straight" {
                let next = (Some (0, 1), (1, 1)) ||> nextPositions sampleMap
                Expect.equal next [| (1, 2) |] ""
            }

            test "correct when at a branch point" {
                let next = (Some (4, 3), (5, 3)) ||> nextPositions sampleMap
                Expect.equal next [| (5, 4); (6, 3) |] ""
            }

        ]

        test "hikeLengths gives correct values" {
            let lengths = PartOne.hikeLengths sampleLines |> Array.sort
            Expect.equal lengths [| 74; 82; 82; 86; 90; 94 |] ""
        }

    ]

    let all = testList "Day 23" [
        sampleInput
    ]