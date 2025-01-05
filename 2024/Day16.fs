module Day16

let parse (lines: string array) =
    lines |> Array.map Seq.toArray |> array2D

type Heading =
    | East
    | North
    | West
    | South

let perpendicularTo h =
    match h with
    | East
    | West -> [| North; South |]
    | North
    | South -> [| East; West |]

let directionVector h =
    match h with
    | East -> 0, 1
    | North -> -1, 0
    | West -> 0, -1
    | South -> 1, 0

type Node = { Pos: int * int; Facing: Heading }

let possibleNextStates allPositions ({ Pos = pos; Facing = f } as node, score) = [
    let ahead = pos |> Pair.add (directionVector f)

    if allPositions |> Set.contains ahead then
        { node with Pos = ahead }, score + 1

    yield!
        f
        |> perpendicularTo
        |> Array.map (fun h -> { node with Facing = h }, score + 1000)
]

module PartOne =

    let solve (lines: string array) =
        let maze = lines |> parse
        let indexed = maze |> Array2D.indexed

        let startNode =
            indexed
            |> Array.find (snd >> (=) 'S')
            |> fst
            |> fun pos -> { Pos = pos; Facing = East }

        let allPositions = indexed |> Array.filter (snd >> (<>) '#') |> Array.map fst |> set

        let nodeToLowScore =
            (Map [ startNode, 0 ], [ startNode, 0 ])
            |> Array.unfold (fun (nodeToLowScore, states) ->
                match states with
                | [] -> None
                | _ ->
                    let possible = states |> List.collect (possibleNextStates allPositions)

                    ((nodeToLowScore, []), possible)
                    ||> List.fold (fun (m, added) (n, s) ->
                        let sIsLowestScoreForN =
                            m
                            |> Map.tryFind n
                            |> Option.map (fun low -> low > s)
                            |> Option.defaultValue true

                        if sIsLowestScoreForN then
                            m |> Map.add n s, (n, s) :: added
                        else
                            m, added)
                    |> fun (newMap, added) -> Some(newMap, (newMap, added)))
            |> Array.last

        indexed
        |> Array.find (snd >> (=) 'E')
        |> fst
        |> fun pos -> [|
            { Pos = pos; Facing = East }
            { Pos = pos; Facing = North }
            { Pos = pos; Facing = West }
            { Pos = pos; Facing = South }
        |]
        |> Array.choose (fun s -> nodeToLowScore |> Map.tryFind s)
        |> Array.min

module PartTwo =

    let solve (lines: string array) = -1

module Test =

    open Expecto
    open Swensen.Unquote

    let all =
        testList "Day16" [
            let sampleInput = [|
                "###############"
                "#.......#....E#"
                "#.#.###.#.###.#"
                "#.....#.#...#.#"
                "#.###.#####.#.#"
                "#.#.#.......#.#"
                "#.#.#####.###.#"
                "#...........#.#"
                "###.#.#####.#.#"
                "#...#.....#.#.#"
                "#.#.#.###.#.#.#"
                "#.....#...#.#.#"
                "#.###.#.#.#.#.#"
                "#S..#.....#...#"
                "###############"
            |]

            let secondSampleInput = [|
                "#################"
                "#...#...#...#..E#"
                "#.#.#.#.#.#.#.#.#"
                "#.#.#.#...#...#.#"
                "#.#.#.#.###.#.#.#"
                "#...#.#.#.....#.#"
                "#.#.#.#.#.#####.#"
                "#.#...#.#.#.....#"
                "#.#.#####.#.###.#"
                "#.#.#.......#...#"
                "#.#.###.#####.###"
                "#.#.#...#.....#.#"
                "#.#.#.#####.###.#"
                "#.#.#.........#.#"
                "#.#.#.#########.#"
                "#S#.............#"
                "#################"
            |]

            testList "PartOne" [
                testCase "solve works with sample input" (fun _ -> test <@ PartOne.solve sampleInput = 7036 @>)
                testCase "solve works with second sample input" (fun _ ->
                    test <@ PartOne.solve secondSampleInput = 11048 @>)
            ]

            testList "PartTwo" [
                testCase "solve works with sample input" (fun _ -> test <@ PartTwo.solve sampleInput = -1 @>)
            ]
        ]

let dayFns = {
    Tests = Test.all
    UtilTests = []
    PartOne = PartOne.solve >> int64
    PartTwo = PartTwo.solve >> int64
}
