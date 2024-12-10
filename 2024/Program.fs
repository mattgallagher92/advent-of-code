open Expecto

[<EntryPoint>]
let main args =

    Day5.Test.all |> runTestsWithCLIArgs [] args |> ignore

    let input = "./input/day05" |> System.IO.File.ReadAllLines

    input |> Day5.PartOne.solve |> printfn "Day 5 part one: %A"
    // input |> Day5.PartTwo.solve |> printfn "Day 5 part two: %A"

    0
