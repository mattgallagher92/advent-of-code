module Day7

type Equation = {
    TestValue: int64
    Numbers: int64 array
}

let parse (lines: string array) =
    lines
    |> Array.map (fun s ->
        let tokens = s.Split ':'

        {
            TestValue = int64 tokens[0]
            Numbers = tokens[1].Trim().Split ' ' |> Array.map int64
        })

let couldBeTrue (operators: (int64 -> int64 -> int64) array) equation =
    ([| Array.head equation.Numbers |], Array.tail equation.Numbers)
    ||> Array.fold (fun possibleVals n ->
        possibleVals
        |> Array.collect (fun v -> operators |> Array.map (fun op -> op v n)))
    |> Array.exists ((=) equation.TestValue)

let solve filter lines =
    lines |> parse |> Array.filter filter |> Array.sumBy _.TestValue

module PartOne =

    let couldBeTrue = couldBeTrue [| (+); (*) |]

    let solve = solve couldBeTrue

module PartTwo =

    // Hacky but works.
    let (||!) i j = int64 $"%i{i}%i{j}"

    let couldBeTrue = couldBeTrue [| (+); (*); (||!) |]

    let solve = solve couldBeTrue

module Test =

    open Expecto
    open Swensen.Unquote

    let all =
        testList "Day7" [
            let sampleInput = [|
                "190: 10 19"
                "3267: 81 40 27"
                "83: 17 5"
                "156: 15 6"
                "7290: 6 8 6 15"
                "161011: 16 10 13"
                "192: 17 8 14"
                "21037: 9 7 18 13"
                "292: 11 6 16 20"
            |]

            testList "PartOne" [
                testCase "solve works with sample input" (fun _ -> test <@ PartOne.solve sampleInput = 3749 @>)
            ]

            testList "PartTwo" [
                testCase "solve works with sample input" (fun _ -> test <@ PartTwo.solve sampleInput = 11387 @>)
            ]
        ]

let dayFns = {
    Tests = Test.all
    UtilTests = []
    PartOne = PartOne.solve
    PartTwo = PartTwo.solve
}
