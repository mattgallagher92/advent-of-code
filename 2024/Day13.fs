module Day13

let regexMatch pattern input =
    System.Text.RegularExpressions.Regex.Match(input, pattern)

type Machine = {
    AMove: int64 * int64
    BMove: int64 * int64
    PrizeLocation: int64 * int64
}

type Play = { APresses: int64; BPresses: int64 }

let cost { APresses = a; BPresses = b } = 3L * a + b

module PartOne =

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

    let parse lines =
        PartOne.parse lines
        |> Array.map (fun m -> {
            m with
                PrizeLocation = let px, py = m.PrizeLocation in px + 10000000000000L, py + 10000000000000L
        })

    let quotientIfNoRemainder (divisor: int64) dividend =
        match System.Math.DivRem(dividend, divisor) with
        | q, 0L -> Some q
        | _ -> None

    let cheapestWin ({ AMove = ax, ay; BMove = bx, by; PrizeLocation = px, py } as machine) =
        let a, b, c, d = ax, bx, ay, by

        let determinant =
            a * d - b * c
            |> fun d ->
                if d = 0L then
                    failwith $"%A{machine} has non-invertible matrix"
                else
                    d

        let winA = d * px - b * py |> quotientIfNoRemainder determinant
        let winB = -c * px + a * py |> quotientIfNoRemainder determinant

        match winA, winB with
        | Some winA, Some winB when winA >= 0L && winB >= 0L -> Some { APresses = winA; BPresses = winB }
        | _ -> None

    let solve (lines: string array) =
        lines |> parse |> Array.choose cheapestWin |> Array.sumBy cost

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
                testCase "cheapestWin returns None, Some _, None, Some _ for sample input" (fun _ ->
                    let result = sampleInput |> PartTwo.parse |> Array.map PartTwo.cheapestWin
                    test <@ result[0].IsNone && result[1].IsSome && result[2].IsNone && result[3].IsSome @>)

                testCase "cheapestWin works with data for part one" (fun _ ->
                    let result = sampleInput |> PartOne.parse |> Array.map PartTwo.cheapestWin

                    test
                        <@
                            result = [|
                                Some { APresses = 80L; BPresses = 40L }
                                None
                                Some { APresses = 38L; BPresses = 86L }
                                None
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
