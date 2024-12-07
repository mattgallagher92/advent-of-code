module Day1

module PartOne =

    let parse lines =
        lines
        |> Array.map (fun (line: string) ->
            match line.Split "   " with
            | [| l; r |] ->
                (Int32.tryParse l, Int32.tryParse r)
                ||> Option.map2 (fun x y -> x, y)
                |> Option.defaultWith (fun _ -> failwith $"Line does not contain ints: %s{line}")
            | _ -> failwith $"Invalid line: %s{line}")
        |> Array.unzip

    let solve (lines: string array) =
        lines
        |> parse
        |> fun (left, right) -> Array.sort left, Array.sort right
        ||> Array.map2 (fun l r -> System.Math.Abs(l - r))
        |> Array.sum

module Test =
    open Expecto
    open Swensen.Unquote

    let all =
        testList "Day 1" [
            let sampleInput = [| "3   4"; "4   3"; "2   5"; "1   3"; "3   9"; "3   3" |]

            testCase "PartOne.parse works" (fun _ ->
                let expected = [| 3; 4; 2; 1; 3; 3 |], [| 4; 3; 5; 3; 9; 3 |]
                test <@ PartOne.parse sampleInput = expected @>)

            testCase "PartOne.solve works" (fun _ -> test <@ PartOne.solve sampleInput = 11 @>)
        ]
