open Expecto

let dayFns day =
    match day with
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
