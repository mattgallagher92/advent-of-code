open Expecto

[<EntryPoint>]
let main args =
    Day24.Test.all |> runTestsWithCLIArgs [] args |> ignore

    let input = "./input/day24" |> System.IO.File.ReadAllLines

    input |> Day24.PartOne.solve 200000000000000.0 400000000000000.0 |> printfn "Day 24 part one: %i"
    // input |> Day24.PartTwo.solve |> printfn "Day 24 part two: %A"

    0
