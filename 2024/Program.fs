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

    testList "All" [ Graph.Tests.all; fns.Tests ]
    |> runTestsWithCLIArgs [] rest
    |> ignore

    // Files in ./input/ are fsproj content copied into the app context directory.
    let input = fns.ReadInput $"%s{System.AppContext.BaseDirectory}/input/day%02i{day}"

    input |> fns.PartOne |> printfn "Day %i part one: %A" day
    input |> fns.PartTwo |> printfn "Day %i part two: %A" day

    0
