open Expecto

let dayFns day =
    match day with
    | 01 -> Day01.dayFns
    | 02 -> Day02.dayFns
    | 03 -> Day03.dayFns
    | 04 -> Day04.dayFns
    | 05 -> Day05.dayFns
    | 06 -> Day06.dayFns
    | 07 -> Day07.dayFns
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
