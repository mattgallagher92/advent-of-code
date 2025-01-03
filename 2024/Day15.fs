module Day15

// TODO: would be nice to make into type extensions, but I can't find the right syntax.
let addPair (i2, j2) (i1, j1) = i1 + i2, j1 + j2

module PartOne =

    let parse (lines: string array) =
        lines
        |> Array.splitAt (lines |> Array.findIndex System.String.IsNullOrEmpty)
        |> fun (a, b) -> a |> Array.map Seq.toArray |> array2D, System.String.Join("", b) |> Seq.toArray

    let attemptMove map (r, c as pos) move =
        let direction =
            match move with
            | '^' -> -1, 0
            | '<' -> 0, -1
            | 'v' -> 1, 0
            | '>' -> 0, 1
            | c -> failwith $"Invalid move char '{c}'"

        let untilStopChar =
            (pos, true)
            |> Array.unfold (fun (pos, b) ->
                if not b then
                    None
                else
                    let x = pos ||> Array2D.get map
                    Some((pos, x), (pos |> addPair direction, x <> '#' && x <> '.')))

        match untilStopChar |> Array.last |> snd with
        | '#' -> map, pos
        | '.' ->
            untilStopChar
            |> Array.pairwise
            |> fun ps -> map, ps
            ||> Array.fold (fun m ((_, x), ((i, j), _)) -> m |> Array2D.updateAt i j x)
            |> fun m -> m |> Array2D.updateAt r c '.'
            |> fun newMap -> newMap, pos |> addPair direction
        | _ -> failwith "Bug"

    let solve (lines: string array) =
        lines
        |> parse
        |> Pair.mapFst (fun map -> map, map |> Array2D.findIndex ((=) '@'))
        ||> Array.fold (fun (map, (r, c)) move -> attemptMove map (r, c) move)
        |> fst
        |> Array2D.findAllIndexes ((=) 'O')
        |> Array.sumBy (fun (r, c) -> 100 * r + c)

module PartTwo =

    let solve (lines: string array) = -1

module Test =

    open Expecto
    open Swensen.Unquote

    let all =
        testList "Day15" [
            let smallSampleInput = [|
                "########"
                "#..O.O.#"
                "##@.O..#"
                "#...O..#"
                "#.#.O..#"
                "#...O..#"
                "#......#"
                "########"
                ""
                "<^^>>>vv<v>>v<<"
            |]

            let largeSampleInput = [|
                "##########"
                "#..O..O.O#"
                "#......O.#"
                "#.OO..O.O#"
                "#..O@..O.#"
                "#O#..O...#"
                "#O..O..O.#"
                "#.OO.O.OO#"
                "#....O...#"
                "##########"
                ""
                "<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^"
                "vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v"
                "><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<"
                "<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^"
                "^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><"
                "^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^"
                ">^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^"
                "<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>"
                "^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>"
                "v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"
            |]

            testList "PartOne" [
                testCase "solve works with small sample" (fun _ -> test <@ PartOne.solve smallSampleInput = 2028 @>)
                testCase "solve works with large sample" (fun _ -> test <@ PartOne.solve largeSampleInput = 10092 @>)
            ]

            testList "PartTwo" [
                testCase "solve works with sample input" (fun _ -> test <@ PartTwo.solve smallSampleInput = -1 @>)
            ]
        ]

let dayFns = {
    Tests = Test.all
    UtilTests = []
    PartOne = PartOne.solve >> int64
    PartTwo = PartTwo.solve >> int64
}
