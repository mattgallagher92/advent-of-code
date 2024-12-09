module Day5

type PageOrderingRule = { Before: int; After: int }

type Input = {
    PageOrderingRules: PageOrderingRule array
    Updates: int array array
}

module PartOne =

    let parse (lines: string array) =
        let blankLineIndex = lines |> Array.findIndex System.String.IsNullOrWhiteSpace
        let orderingLines = lines |> Array.take blankLineIndex
        let updateLines = lines |> Array.skip (blankLineIndex + 1)

        {
            PageOrderingRules =
                orderingLines
                |> Array.map (fun l ->
                    let tokens = l.Split '|'

                    {
                        Before = int tokens[0]
                        After = int tokens[1]
                    })
            Updates = updateLines |> Array.map (fun l -> l.Split ',' |> Array.map int)
        }

module Test =
    open Expecto
    open Swensen.Unquote

    let all =
        testList "Day 5" [
            testList "PartOne" [
                let sampleInput = [|
                    "47|53"
                    "97|13"
                    "97|61"
                    "97|47"
                    "75|29"
                    "61|13"
                    "75|53"
                    "29|13"
                    "97|29"
                    "53|29"
                    "61|53"
                    "97|53"
                    "61|29"
                    "47|13"
                    "75|47"
                    "97|75"
                    "47|61"
                    "75|61"
                    "47|29"
                    "75|13"
                    "53|13"
                    ""
                    "75,47,61,53,29"
                    "97,61,53,29,13"
                    "75,29,13"
                    "75,97,47,61,53"
                    "61,13,29"
                    "97,13,75,29,47"
                |]

                testCase "PartOne.parse works with sample input" (fun _ ->
                    let expected = {
                        PageOrderingRules = [|
                            { Before = 47; After = 53 }
                            { Before = 97; After = 13 }
                            { Before = 97; After = 61 }
                            { Before = 97; After = 47 }
                            { Before = 75; After = 29 }
                            { Before = 61; After = 13 }
                            { Before = 75; After = 53 }
                            { Before = 29; After = 13 }
                            { Before = 97; After = 29 }
                            { Before = 53; After = 29 }
                            { Before = 61; After = 53 }
                            { Before = 97; After = 53 }
                            { Before = 61; After = 29 }
                            { Before = 47; After = 13 }
                            { Before = 75; After = 47 }
                            { Before = 97; After = 75 }
                            { Before = 47; After = 61 }
                            { Before = 75; After = 61 }
                            { Before = 47; After = 29 }
                            { Before = 75; After = 13 }
                            { Before = 53; After = 13 }
                        |]
                        Updates = [|
                            [| 75; 47; 61; 53; 29 |]
                            [| 97; 61; 53; 29; 13 |]
                            [| 75; 29; 13 |]
                            [| 75; 97; 47; 61; 53 |]
                            [| 61; 13; 29 |]
                            [| 97; 13; 75; 29; 47 |]
                        |]
                    }

                    test <@ PartOne.parse sampleInput = expected @>)
            ]
        ]
