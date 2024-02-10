module Day23

let startPosition lines = lines |> Array.head |> Seq.findIndex ((=) '.') |> fun col -> 0, col

let above map (row, col) = if row = 0 then None else Array2D.get map (row - 1) col |> Some
let below map (row, col) = if row = Array2D.length1 map - 1 then None else Array2D.get map (row + 1) col |> Some
let toLeft map (row, col) = if col = 0 then None else Array2D.get map row (col - 1) |> Some
let toRight map (row, col) = if col = Array2D.length2 map - 1 then None else Array2D.get map row (col + 1) |> Some

let adjacentPositions map position =
    [|
        above map position
        below map position
        toLeft map position
        toRight map position
    |]
    |> Array.choose (Option.bind (fun c -> if c = '#' then None else Some c))

/// Finds the positions corresponding to places where a decision about which direction to follow has to be made.
let findBranchPoints map = [|
    for row in 0 .. Array2D.length1 map - 1 do
        for col in 0 .. Array2D.length2 map - 1 do
            if Array2D.get map row col = '.' && Array.length (adjacentPositions map (row, col)) > 2 then
                 row, col
|]

module PartOne =

    let solve lines =

        0

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
                let x = toLeft sampleMap (3, 11)
                Expect.equal x (Some '>') ""
            }

            test "toRight" {
                let x = toRight sampleMap (3, 11)
                Expect.equal x (Some '>') ""
            }
        ]

        test "correct branch points" {
            let branchPoints = sampleMap |> findBranchPoints
            let expected = [| 3, 11; 5, 3; 11, 21; 13, 5; 13, 13; 19, 13; 19, 19 |]
            Expect.equal branchPoints expected ""
        }

    ]

    let all = testList "Day 23" [
        sampleInput
    ]