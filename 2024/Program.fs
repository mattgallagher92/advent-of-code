open Expecto

[<EntryPoint>]
let main args =

    Day3.Test.all |> runTestsWithCLIArgs [] args |> ignore

    let input = "./input/day03" |> System.IO.File.ReadAllLines

    // input |> Day3.PartOne.solve |> printfn "Day 3 part one: %A"
    // input |> Day3.PartTwo.solve |> printfn "Day 3 part two: %A"

    0
