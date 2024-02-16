open Expecto
open MathNet.Numerics

[<EntryPoint>]
let main args =
    Day24.Test.all |> runTestsWithCLIArgs [] args |> ignore

    let input = "./input/day24" |> System.IO.File.ReadAllLines

    input
    |> Day24.PartOne.solve (BigRational.FromDecimal 200000000000000m) (BigRational.FromDecimal 400000000000000m)
    |> printfn "Day 24 part one: %i"

    input |> Day24.PartTwo.solve |> printfn "Day 24 part two: %A"

    0
