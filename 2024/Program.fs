open Expecto

[<EntryPoint>]
let main args =

    Day2.Test.all |> runTestsWithCLIArgs [] args |> ignore

    let input = "./input/day2" |> System.IO.File.ReadAllLines

    input |> Day2.PartOne.solve |> printfn "Day 2 part one: %A"
    input |> Day2.PartTwo.solve |> printfn "Day 2 part two: %A"

    0
