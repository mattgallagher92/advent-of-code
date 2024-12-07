open Expecto

[<EntryPoint>]
let main args =

    Day1.Test.all |> runTestsWithCLIArgs [] args |> ignore

    let input = "./input/day1" |> System.IO.File.ReadAllLines

    input |> Day1.PartOne.solve |> printfn "Day 1 part one: %A"

    0
