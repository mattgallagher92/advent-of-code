open Expecto

[<EntryPoint>]
let main args =

    Day25.Test.all |> runTestsWithCLIArgs [] args |> ignore

    let input = "./input/day25" |> System.IO.File.ReadAllLines

    // input |> Day25.PartOne.solve |> printfn "Day 25 part two: %A"
    // input |> Day25.PartTwo.solve |> printfn "Day 25 part two: %A"

    0
