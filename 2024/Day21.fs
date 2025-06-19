module Day21

[<Literal>]
let Up = "^"

[<Literal>]
let Down = "v"

[<Literal>]
let Left = "<"

[<Literal>]
let Right = ">"

let numericRow =
    function
    | '0' -> 0
    | 'A' -> 0
    | '1' -> 1
    | '2' -> 1
    | '3' -> 1
    | '4' -> 2
    | '5' -> 2
    | '6' -> 2
    | '7' -> 3
    | '8' -> 3
    | '9' -> 3
    | c -> failwith $"Invalid numeric button %c{c}"

let numericCol =
    function
    | '0' -> 1
    | 'A' -> 2
    | '1' -> 0
    | '2' -> 1
    | '3' -> 2
    | '4' -> 0
    | '5' -> 1
    | '6' -> 2
    | '7' -> 0
    | '8' -> 1
    | '9' -> 2
    | c -> failwith $"Invalid numeric button %c{c}"

let directionalRow c =
    match $"%c{c}" with
    | Left -> 0
    | Down -> 0
    | Right -> 0
    | Up -> 1
    | "A" -> 1
    | s -> failwith $"Invalid directional button %s{s}"

let directionalCol c =
    match $"%c{c}" with
    | Left -> 0
    | Down -> 1
    | Right -> 2
    | Up -> 1
    | "A" -> 2
    | s -> failwith $"Invalid directional button %s{s}"

let (|Positive|Zero|Negative|) x =
    if x > 0 then Positive
    elif x = 0 then Zero
    else Negative

module PartOne =

    /// Returns a string representing all movements in each required direction, one direction at a time, to minimise
    /// the amount of button presses required in the subsequent expansions.
    let shortestPathNumeric (x, y) =
        let rX, cX = numericRow x, numericCol x
        let rY, cY = numericRow y, numericCol y
        let rDiff = rY - rX
        let cDiff = cY - cX

        let up () = String.replicate rDiff Up
        let down () = String.replicate -rDiff Down
        let right () = String.replicate cDiff Right
        let left () = String.replicate -cDiff Left

        let move =
            // To keep the next expansions as small as possible: if the move requires multiple directions, make all
            // moves in one direction followed by all moves in the other; prefer to go left first, then up/down,
            // then right, unless doing so would result in going over a blank space on the grid. The direction
            // preference can be seen by trying alternative orders and comparing the results after a few expansions.
            match rDiff, cDiff with
            | Positive, Positive -> $"%s{up ()}%s{right ()}"
            | Positive, Zero -> $"%s{up ()}"
            | Positive, Negative when rX = 0 && cY = 0 -> $"%s{up ()}%s{left ()}"
            | Positive, Negative -> $"%s{left ()}%s{up ()}"
            | Zero, Positive -> $"%s{right ()}"
            | Zero, Zero -> ""
            | Zero, Negative -> $"%s{left ()}"
            | Negative, Positive when cX = 0 && rY = 0 -> $"%s{right ()}%s{down ()}"
            | Negative, Positive -> $"%s{down ()}%s{right ()}"
            | Negative, Zero -> $"%s{down ()}"
            | Negative, Negative -> $"%s{left ()}%s{down ()}"

        $"%s{move}A"

    /// Returns a string representing all movements in each required direction, one direction at a time, to minimise
    /// the amount of button presses required in the subsequent expansions.
    let shortestPathDirectional (x, y) =
        let rX, cX = directionalRow x, directionalCol x
        let rY, cY = directionalRow y, directionalCol y
        let rDiff = rY - rX
        let cDiff = cY - cX

        let up () = String.replicate rDiff Up
        let down () = String.replicate -rDiff Down
        let right () = String.replicate cDiff Right
        let left () = String.replicate -cDiff Left

        let move =
            // To keep the next expansions as small as possible: if the move requires multiple directions, make all
            // moves in one direction followed by all moves in the other; prefer to go left first, then up/down,
            // then right, unless doing so would result in going over a blank space on the grid. The direction
            // preference can be seen by trying alternative oreders and comparing the results after a few expansions.
            match rDiff, cDiff with
            | Positive, Positive when x = '<' -> $"%s{right ()}%s{up ()}"
            | Positive, Positive -> $"%s{up ()}%s{right ()}"
            | Positive, Zero -> $"%s{up ()}"
            | Positive, Negative -> $"%s{left ()}%s{up ()}"
            | Zero, Positive -> $"%s{right ()}"
            | Zero, Zero -> ""
            | Zero, Negative -> $"%s{left ()}"
            | Negative, Positive -> $"%s{down ()}%s{right ()}"
            | Negative, Zero -> $"%s{down ()}"
            | Negative, Negative when y = '<' -> $"%s{down ()}%s{left ()}"
            | Negative, Negative -> $"%s{left ()}%s{down ()}"

        $"%s{move}A"

    let expandNumeric numericButtonPresses =
        // Start at A.
        $"A%s{numericButtonPresses}"
        |> Seq.pairwise
        |> Seq.map shortestPathNumeric
        |> String.concat ""

    let expandDirectional (directionalButtonPresses: string) =
        // Start at A.
        $"A%s{directionalButtonPresses}"
        |> Seq.pairwise
        |> Seq.map shortestPathDirectional
        |> String.concat ""

    let fullyExpand = expandNumeric >> expandDirectional >> expandDirectional

    let complexity code =
        code |> fullyExpand |> _.Length |> (*) (int code[0..2])

    let solve (lines: string array) = lines |> Array.sumBy complexity

module PartTwo =

    let solve (lines: string array) = -1

module Test =

    open Expecto
    open Swensen.Unquote

    let all =
        testList "Day21" [
            let sampleInput = [| "029A"; "980A"; "179A"; "456A"; "379A" |]

            testList "PartOne" [
                testCase "expandNumeric works" (fun _ ->
                    let possibilities = [| "<A^A>^^AvvvA"; "<A^A^>^AvvvA"; "<A^A^^>AvvvA" |]

                    test <@ possibilities |> Array.contains (PartOne.expandNumeric "029A") @>)

                testCase "complexity works with sample input" (fun _ ->
                    test
                        <@
                            sampleInput |> Array.map PartOne.complexity = [|
                                68 * 29
                                60 * 980
                                68 * 179
                                64 * 456
                                64 * 379
                            |]
                        @>)
                testCase "solve works with sample input" (fun _ -> test <@ PartOne.solve sampleInput = 126384 @>)
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
