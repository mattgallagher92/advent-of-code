open Expecto

[<EntryPoint>]
let main args =
    Day23.Test.all |> runTestsWithCLIArgs [] args |> ignore

    let input = "./input/day23" |> System.IO.File.ReadAllLines

    input |> Day23.PartOne.solve |> printfn "Day 23 part one: %i"
    // input |> Day23.PartTwo.solve |> printfn "Day 23 part two: %A"

    0
