module Day15

// TODO: would be nice to make into type extensions, but I haven't found the right syntax.
let addPair (i2, j2) (i1, j1) = i1 + i2, j1 + j2

let direction =
    function
    | '^' -> -1, 0
    | '<' -> 0, -1
    | 'v' -> 1, 0
    | '>' -> 0, 1
    | c -> failwith $"Invalid move char '{c}'"

module PartOne =

    let parse (lines: string array) =
        lines
        |> Array.splitAt (lines |> Array.findIndex System.String.IsNullOrEmpty)
        |> fun (a, b) -> a |> Array.map Seq.toArray |> array2D, System.String.Join("", b) |> Seq.toArray

    let attemptMove map (r, c as pos) move =
        let direction = direction move

        let untilStopChar =
            (pos, true)
            |> Array.unfold (fun (p, b) ->
                if not b then
                    None
                else
                    let x = p ||> Array2D.get map
                    Some((p, x), (p |> addPair direction, x <> '#' && x <> '.')))

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

    let expand =
        function
        | '#' -> [| '#'; '#' |]
        | 'O' -> [| '['; ']' |]
        | '.' -> [| '.'; '.' |]
        | '@' -> [| '@'; '.' |]
        | c -> failwith $"Invalid input char {c}"

    let parse (lines: string array) =
        lines
        |> Array.splitAt (lines |> Array.findIndex System.String.IsNullOrEmpty)
        |> fun (a, b) ->
            a |> Array.map (Seq.collect expand >> Seq.toArray) |> array2D, System.String.Join("", b) |> Seq.toArray

    let attemptMove map pos move =
        let d = direction move

        let acc =
            ([| pos |], true)
            |> Array.unfold (fun (ps, canContinue) ->
                if not canContinue then
                    None
                else
                    let mutable hitBox = false
                    let mutable blocked = false

                    let next =
                        ps
                        |> Array.collect (fun p ->
                            let aheadPos = p |> addPair d

                            match aheadPos ||> Array2D.get map with
                            | '.' -> [||]
                            | '[' ->
                                hitBox <- true

                                match d with
                                | _, 0 -> [| aheadPos; aheadPos |> addPair (0, 1) |]
                                | 0, 1
                                | 0, -1 -> [| aheadPos |]
                                | _ -> failwith $"Invalid direction vector %A{d}"
                            | ']' ->
                                hitBox <- true

                                match d with
                                | _, 0 -> [| aheadPos; aheadPos |> addPair (0, -1) |]
                                | 0, 1
                                | 0, -1 -> [| aheadPos |]
                                | _ -> failwith $"Invalid direction vector %A{d}"
                            | '#' ->
                                blocked <- true
                                [||]
                            | c -> failwith $"Invalid ahead char {c}")
                        // Can end up with duplicate positions if both the left and right hand side of the box are pushed.
                        |> Array.distinct

                    Some((ps, blocked), (next, hitBox && not blocked)))

        let isBlocked = acc |> Array.last |> snd

        if isBlocked then
            map, pos
        else
            acc
            |> Array.collect fst
            |> fun positionsToMove -> positionsToMove, map
            ||> Array.foldBack (fun (r, c as p) m ->
                let p' = p |> addPair d
                let r', c' = p'
                m |> Array2D.updateAt r' c' (m[r, c]) |> Array2D.updateAt r c '.')
            |> fun m -> m, pos |> addPair d

    let solve (lines: string array) =
        let sb = System.Text.StringBuilder()

        let result =
            lines
            |> parse
            |> Pair.mapFst (fun map -> map, map |> Array2D.findIndex ((=) '@'))
            ||> Array.fold (fun (map, pos) move -> attemptMove map pos move)
            |> fst
            |> Array2D.findAllIndexes ((=) '[')
            |> Array.sumBy (fun (r, c) -> 100 * r + c)

        System.IO.File.WriteAllText("./maps.txt", sb.ToString())

        result

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
                testCase "move up works with two boxes ahead" (fun _ ->
                    let map =
                        PartTwo.parse [| "###"; "#.#"; "#O#"; "#O#"; "#@#"; "###"; ""; "^" |] |> fst

                    let initialPos = map |> Array2D.findIndex ((=) '@')
                    let newMap, newPos = PartTwo.attemptMove map initialPos '^'

                    let expectedMap =
                        PartTwo.parse [| "###"; "#O#"; "#O#"; "#@#"; "#.#"; "###"; ""; "^" |] |> fst

                    let expectedPos = expectedMap |> Array2D.findIndex ((=) '@')
                    test <@ newMap = expectedMap && newPos = expectedPos @>)

                testCase "solve works with sample input" (fun _ -> test <@ PartTwo.solve largeSampleInput = 9021 @>)
            ]
        ]

let dayFns = {
    Tests = Test.all
    UtilTests = []
    PartOne = PartOne.solve >> int64
    PartTwo = PartTwo.solve >> int64
}
