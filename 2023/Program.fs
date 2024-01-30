open Expecto

[<EntryPoint>]
let main args =
    Day22.Test.all |> runTestsWithCLIArgs [] args |> ignore

    let input = "./input/day22" |> System.IO.File.ReadAllLines

    input |> Day22.PartOne.solve |> printfn "Day 22 part one: %i"
    input |> Day22.PartTwo.solve |> printfn "Day 23 part two: %A"

    0
