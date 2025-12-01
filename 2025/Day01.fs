module Day01

let modulo m x = (x % m + m) % m

let (|Positive|Zero|Negative|) x =
    if x > 0 then Positive
    elif x = 0 then Zero
    else Negative

let parseLine (line: string) =
    match line[0], Int32.tryParse line[1..] with
    | 'L', Some i -> -i
    | 'R', Some i -> i
    | _ -> failwith $"Invalid line %s{line}"

module PartOne =

    let applyRotation state rotation = state + rotation |> modulo 100

    let solve (lines: string array) =
        lines
        |> Array.map parseLine
        |> Array.scan applyRotation 50
        |> Array.filter ((=) 0)
        |> Array.length

module PartTwo =

    // TODO: refactor.
    let applyRotation (timesPastZero, currentPosition) rotation =
        let additionalTimesPastZero =
            let q, r = System.Math.DivRem(rotation, 100)

            let wholeRotations =
                match rotation with
                | Positive -> q
                | Zero -> failwith "broken assumption"
                | Negative -> -q

            let partialRotationsPastZero =
                match r with
                | Positive -> (currentPosition + r) / 100
                | Zero -> 0
                | Negative ->
                    if currentPosition + r <= 0 && currentPosition > 0 then
                        1
                    else
                        0

            wholeRotations + partialRotationsPastZero

        timesPastZero + additionalTimesPastZero, currentPosition + rotation |> modulo 100

    let solve (lines: string array) =
        lines
        |> Array.map parseLine
        |> Array.scan applyRotation (0, 50)
        |> Array.last
        |> fst

module Test =

    open Expecto
    open Swensen.Unquote

    let all =
        testList "DayN" [
            let sampleInput = [| "L68"; "L30"; "R48"; "L5"; "R60"; "L55"; "L1"; "L99"; "R14"; "L82" |]

            testList "PartOne" [
                testCase "solve works with sample input" (fun _ -> test <@ PartOne.solve sampleInput = 3 @>)
            ]

            testList "PartTwo" [
                testCase "solve works with sample input" (fun _ -> test <@ PartTwo.solve sampleInput = 6 @>)
            ]
        ]

let dayFns = {
    Tests = Test.all
    UtilTests = []
    PartOne = PartOne.solve >> int64
    PartTwo = PartTwo.solve >> int64
}
