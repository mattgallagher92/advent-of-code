open Expecto

[<EntryPoint>]
let main args =
    Day22.Test.all |> runTestsWithCLIArgs [] args |> ignore

    "./input/day22"
    |> System.IO.File.ReadAllLines
    |> Day22.PartOne.solve
    |> printfn "Day 22 part one: %i"

    0
