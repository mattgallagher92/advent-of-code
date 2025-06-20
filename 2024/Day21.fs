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
    // To keep the next expansions as small as possible: if the move requires multiple directions, make all
    // moves in one direction followed by all moves in the other; prefer to go left first, then up/down,
    // then right, unless doing so would result in going over a blank space on the grid. The direction
    // preference can be seen by trying alternative orders and comparing the results after a few expansions.
    match x, y with
    | '<', '<' -> ""
    | '<', 'v' -> ">"
    | '<', '^' -> ">^"
    | '<', '>' -> ">>"
    | '<', 'A' -> ">>^"
    | 'v', '<' -> "<"
    | 'v', 'v' -> ""
    | 'v', '^' -> "^"
    | 'v', '>' -> ">"
    | 'v', 'A' -> "^>"
    | '^', '<' -> "v<"
    | '^', 'v' -> "v"
    | '^', '^' -> ""
    | '^', '>' -> "v>"
    | '^', 'A' -> ">"
    | '>', '<' -> "<<"
    | '>', 'v' -> "<"
    | '>', '^' -> "<^"
    | '>', '>' -> ""
    | '>', 'A' -> "^"
    | 'A', '<' -> "v<<"
    | 'A', 'v' -> "<v"
    | 'A', '^' -> "<"
    | 'A', '>' -> "v"
    | 'A', 'A' -> ""
    | _ -> failwith $"Invalid directional buttons (%c{x}, %c{y})"
    |> fun move -> $"%s{move}A"

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

module PartOne =


    let fullyExpand = expandNumeric >> expandDirectional >> expandDirectional

    let complexity code =
        code |> fullyExpand |> _.Length |> (*) (int code[0..2])

    let solve (lines: string array) = lines |> Array.sumBy complexity

module PartTwo =

    [<Literal>]
    let s01 = "A"

    [<Literal>]
    let s02 = "<A"

    [<Literal>]
    let s03 = "<<A"

    [<Literal>]
    let s04 = "<^A"

    [<Literal>]
    let s05 = "<vA"

    [<Literal>]
    let s06 = ">A"

    [<Literal>]
    let s07 = ">>A"

    [<Literal>]
    let s08 = ">>^A"

    [<Literal>]
    let s09 = ">^A"

    [<Literal>]
    let s10 = "^A"

    [<Literal>]
    let s11 = "^>A"

    [<Literal>]
    let s12 = "vA"

    [<Literal>]
    let s13 = "v<A"

    [<Literal>]
    let s14 = "v<<A"

    [<Literal>]
    let s15 = "v>A"

    let ss = [| s01; s02; s03; s04; s05; s06; s07; s08; s09; s10; s11; s12; s13; s14; s15 |]

    open MathNet.Numerics.LinearAlgebra

    /// Returns a vector whose (1-based) NNth component is the number of occurrences of sNN in moveSequence.
    ///
    /// More info: the `sNN`s are precisely the strings that represent the required key presses on one directional
    /// keypad to cause the robot it controls to press one button on its directional keypad after having pressed
    /// another. As a result, a string representing the key presses required to instruct a robot to press keys on a
    /// directional keypad can be broken down into a sequence of `sNN`s. Because in the end we're only interested in
    /// the length of such a string, the order of the `sNN`s that make up the string doesn't matter.
    let calculateRepresentation (moveSequence: string) =
        let occurenceCounts =
            moveSequence.Split 'A'
            |> Array.map (fun s -> $"%s{s}A")
            |> fun a -> a[0 .. a.Length - 2]
            |> Array.countBy id
            |> dict

        ss
        |> Array.map (occurenceCounts.TryGet >> Option.defaultValue 0 >> float)
        |> vector

    /// The lengths of s01 to s15.
    let lengths = ss |> Array.map (_.Length >> float) |> vector

    /// The matrix whose JJth column is the vector representation of the number of occurrences of the sNN in the
    /// sequence of button presses required to cause the controlled robot to enter sJJ on its keypad. As a result,
    /// it is the matrix that, when applied to a representation vector, gives the representation vector for the
    /// keypresses required on the upstream keypad.
    let expansionMatrix =
        ss
        |> Array.map (expandDirectional >> calculateRepresentation)
        |> matrix
        |> Matrix.transpose

    /// n is the number of robots using directional keypads.
    let complexity n code =
        // n - 1 because we apply one expandDirectional below.
        let m = expansionMatrix.Power(n - 1)

        code
        |> expandNumeric
        // Apply an expandDirectional so that the sequence can be broken down into the `sNN`s.
        |> expandDirectional
        |> calculateRepresentation
        // Convert to the representation of the final sequence.
        |> m.Multiply
        // Convert to the length of the underlying sequence.
        |> lengths.DotProduct
        |> int64
        |> (*) (int64 code[0..2])

    let solve (lines: string array) = lines |> Array.sumBy (complexity 25)

module Test =

    open Expecto
    open Swensen.Unquote

    let all =
        testList "Day21" [
            testCase "expandNumeric works" (fun _ ->
                let possibilities = [| "<A^A>^^AvvvA"; "<A^A^>^AvvvA"; "<A^A^^>AvvvA" |]

                test <@ possibilities |> Array.contains (expandNumeric "029A") @>)

            let sampleInput = [| "029A"; "980A"; "179A"; "456A"; "379A" |]

            testList "PartOne" [
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
                testCase "complexity works with sample input" (fun _ ->
                    test
                        <@
                            sampleInput |> Array.map (PartTwo.complexity 2) = [|
                                68L * 29L
                                60L * 980L
                                68L * 179L
                                64L * 456L
                                64L * 379L
                            |]
                        @>)
            ]
        ]

let dayFns = {
    Tests = Test.all
    UtilTests = []
    PartOne = PartOne.solve >> int64
    PartTwo = PartTwo.solve >> int64
}
