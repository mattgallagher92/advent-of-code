open Expecto

[<EntryPoint>]
let main args =

    Day4.Test.all |> runTestsWithCLIArgs [] args |> ignore

    let input = "./input/day04" |> System.IO.File.ReadAllLines

    input |> Day4.PartOne.solve |> printfn "Day 4 part one: %A"
    // input |> Day4.PartTwo.solve |> printfn "Day 3 part two: %A"

    0
