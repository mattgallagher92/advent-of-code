open Expecto

let tests includeUtil day =
    testList "All" [
        if includeUtil then
            Graph.Tests.all

        match day with
        | 5 -> Day5.Test.all
        | _ -> ()
    ]

[<EntryPoint>]
let main args =

    tests true 5 |> runTestsWithCLIArgs [] args |> ignore

    let input = "./input/day05" |> System.IO.File.ReadAllLines

    input |> Day5.PartOne.solve |> printfn "Day 5 part one: %A"
    input |> Day5.PartTwo.solve |> printfn "Day 5 part two: %A"

    0
