open Expecto

let dayFns day =
    match day with
    | 01 -> Day1.dayFns
    | 02 -> Day2.dayFns
    | 03 -> Day3.dayFns
    | 04 -> Day4.dayFns
    | 05 -> Day5.dayFns
    | 06 -> Day6.dayFns
    | 07 -> Day7.dayFns
    | 08 -> Day8.dayFns
    | 09 -> Day9.dayFns
    | 10 -> Day10.dayFns
    | 11 -> Day11.dayFns
    | 12 -> Day12.dayFns
    | 13 -> Day13.dayFns
    | 14 -> Day14.dayFns
    | 15 -> Day15.dayFns
    | 16 -> Day16.dayFns
    | 17 -> Day17.dayFns
    | 18 -> Day18.dayFns
    | 19 -> Day19.dayFns
    | 20 -> Day20.dayFns
    | 21 -> Day21.dayFns
    | 22 -> Day22.dayFns
    | 23 -> Day23.dayFns
    | 24 -> Day24.dayFns
    | 25 -> Day25.dayFns
    | _ -> failwith $"Unrecognized day %i{day}"

[<EntryPoint>]
let main args =
    let day, rest =
        args
        |> Array.tryHead
        |> Option.bind Int32.tryParse
        |> Option.defaultWith (fun _ -> failwith $"First argument must be a day number.")
        |> fun d -> d, Array.tail args

    let fns = dayFns day

    testList "All" [ testList $"Day %i{day} util tests" fns.UtilTests; fns.Tests ]
    |> runTestsWithCLIArgs [] rest
    |> ignore

    // Files in ./input/ are fsproj content copied into the app context directory.
    let input =
        System.IO.File.ReadAllLines $"%s{System.AppContext.BaseDirectory}/input/day%02i{day}"

    input |> fns.PartOne |> printfn "Day %i part one: %A" day
    input |> fns.PartTwo |> printfn "Day %i part two: %A" day

    0
