module Day13

let regexMatch pattern input =
    System.Text.RegularExpressions.Regex.Match(input, pattern)

type Machine = {
    AMove: int * int
    BMove: int * int
    PrizeLocation: int * int
}

let parse (lines: string array) =
    let behaviourDescriptions =
        lines |> Array.windowed 3 |> Array.withIndexMatching (fun i -> i % 4 = 0)

    behaviourDescriptions
    |> Array.map (function
        | [| a; b; p |] as desc ->
            let aMatch = a |> regexMatch "Button A: X\+(\d+), Y\+(\d+)"
            let bMatch = b |> regexMatch "Button B: X\+(\d+), Y\+(\d+)"
            let pMatch = p |> regexMatch "Prize: X=(\d+), Y=(\d+)"

            match aMatch.Success, bMatch.Success, pMatch.Success with
            | true, true, true -> {
                AMove = int aMatch.Groups[1].Value, int aMatch.Groups[2].Value
                BMove = int bMatch.Groups[1].Value, int bMatch.Groups[2].Value
                PrizeLocation = int pMatch.Groups[1].Value, int pMatch.Groups[2].Value
              }
            | x -> failwith $"Invalid input behaviour description: %A{desc}, {x}"
        | desc -> failwith $"Invalid input behaviour description: %A{desc}")

module PartOne =

    type Play = { APresses: int; BPresses: int }

    let cost { APresses = a; BPresses = b } = 3 * a + b

    let buttonPressPossibilities =
        Array.allPairs [| 0..100 |] [| 0..100 |]
        |> Array.map (fun (a, b) -> { APresses = a; BPresses = b })
        |> Array.sortBy cost

    let cheapestWin { AMove = ax, ay; BMove = bx, by; PrizeLocation = px, py } =
        buttonPressPossibilities
        |> Array.tryFind (fun { APresses = ap; BPresses = bp } -> px = ap * ax + bp * bx && py = ap * ay + bp * by)

    let solve (lines: string array) =
        lines |> parse |> Array.choose cheapestWin |> Array.sumBy cost

module PartTwo =

    let solve (lines: string array) = -1

module Test =

    open Expecto
    open Swensen.Unquote

    let all =
        testList "Day13" [
            let sampleInput = [|
                "Button A: X+94, Y+34"
                "Button B: X+22, Y+67"
                "Prize: X=8400, Y=5400"
                ""
                "Button A: X+26, Y+66"
                "Button B: X+67, Y+21"
                "Prize: X=12748, Y=12176"
                ""
                "Button A: X+17, Y+86"
                "Button B: X+84, Y+37"
                "Prize: X=7870, Y=6450"
                ""
                "Button A: X+69, Y+23"
                "Button B: X+27, Y+71"
                "Prize: X=18641, Y=10279"
            |]

            testList "PartOne" [
                testCase "solve works with sample input" (fun _ -> test <@ PartOne.solve sampleInput = 480 @>)
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
