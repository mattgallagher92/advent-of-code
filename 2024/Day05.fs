module Day5

type PageOrderingRule = { Before: int; After: int }

type Input = {
    PageOrderingRules: PageOrderingRule array
    Updates: int array array
}

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

/// Given an update and a rule, returns whether the update satisfies the rule.
let isSatisfiedBy update =
    // Create dictionaries of pages to their index in each update, for constant-time index lookups.
    let d = update |> Array.indexed |> Array.map (fun (ix, page) -> page, ix) |> dict

    // Return anonymous function to cache dictionary creation when satisfiedBy is partially applied.
    fun { Before = b; After = a } ->
        (d.TryGet b, d.TryGet a)
        ||> Option.map2 (fun beforeIx afterIx -> beforeIx < afterIx)
        |> Option.defaultValue true

/// Given updates and rules, returns those updates that satisfy all of the rules.
let updatesSatisfyingAll updates =
    let s = updates |> Array.map isSatisfiedBy |> Array.indexed

    fun rules ->
        (s, rules)
        ||> Array.fold (fun satisfiedByFuncs rule -> satisfiedByFuncs |> Array.filter (fun (_, s) -> s rule))
        |> Array.map (fun (ix, _) -> updates[ix])

module PartOne =

    let solve (lines: string array) =
        lines
        |> parse
        |> fun
               {
                   Updates = updates
                   PageOrderingRules = rules
               } -> rules |> updatesSatisfyingAll updates |> Array.sumBy (fun u -> u[u.Length / 2])

module Test =
    open Expecto
    open Swensen.Unquote

    let all =
        testList "Day 5" [
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

            let exptectedRules = [|
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

            let expectedUpdates = [|
                [| 75; 47; 61; 53; 29 |]
                [| 97; 61; 53; 29; 13 |]
                [| 75; 29; 13 |]
                [| 75; 97; 47; 61; 53 |]
                [| 61; 13; 29 |]
                [| 97; 13; 75; 29; 47 |]
            |]

            testCase "parse works with sample input" (fun _ ->
                let expected = {
                    PageOrderingRules = exptectedRules
                    Updates = expectedUpdates
                }

                test <@ parse sampleInput = expected @>)

            testCase "updatesSatisfyingAll works with sample input" (fun _ ->
                let expectedResult = [| [| 75; 47; 61; 53; 29 |]; [| 97; 61; 53; 29; 13 |]; [| 75; 29; 13 |] |]
                test <@ updatesSatisfyingAll expectedUpdates exptectedRules = expectedResult @>)

            testCase "PartOne.solve works with sample input" (fun _ -> test <@ PartOne.solve sampleInput = 143 @>)
        ]
