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

    let applyRotation (timesPastZero, currentPosition) rotation =
        match rotation with
        | Positive ->
            let q, r = System.Math.DivRem(currentPosition + rotation, 100)
            timesPastZero + q, r
        | Zero -> failwith "Should not be any zero rotations."
        | Negative ->
            let q, r = System.Math.DivRem((currentPosition - 100) % 100 + rotation, 100)
            timesPastZero - q, (r + 100) % 100

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
        testList
            "Day01"
            [
                let sampleInput =
                    [|
                        "L68"
                        "L30"
                        "R48"
                        "L5"
                        "R60"
                        "L55"
                        "L1"
                        "L99"
                        "R14"
                        "L82"
                    |]

                testList
                    "PartOne"
                    [ testCase "solve works with sample input" (fun _ -> test <@ PartOne.solve sampleInput = 3 @>) ]

                testList
                    "PartTwo"
                    [ testCase "solve works with sample input" (fun _ -> test <@ PartTwo.solve sampleInput = 6 @>) ]
            ]

let dayFns =
    {
        Tests = Test.all
        UtilTests = []
        PartOne = PartOne.solve >> int64
        PartTwo = PartTwo.solve >> int64
    }
