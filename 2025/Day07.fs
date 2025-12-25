module Day07

module PartOne =

    let outputBeamsAndNumSplits (inputBeams: bool array, numSplits) (line: string) =
        let mutable numSplits = numSplits + if inputBeams[0] && line[0] = '^' then 1 else 0

        let outputBeams = [|
            for i in 0 .. inputBeams.Length - 1 do
                match line[i] with
                | 'S' -> true
                | '.' ->
                    let splitLeft =
                        (inputBeams |> Seq.tryItem (i - 1), line |> Seq.tryItem (i - 1))
                        ||> Option.map2 (fun b c -> b && c = '^')
                        |> Option.defaultValue false

                    let splitRight =
                        (inputBeams |> Seq.tryItem (i + 1), line |> Seq.tryItem (i + 1))
                        ||> Option.map2 (fun b c -> b && c = '^')
                        |> Option.defaultValue false

                    if splitRight then
                        numSplits <- numSplits + 1

                    splitLeft || inputBeams[i] || splitRight
                // Assumes that there aren't two splitters next to each other, which is true for sample and my input.
                | '^' -> false
                | _ -> failwith "Bug"
        |]

        outputBeams, numSplits

    let solve (lines: string array) =
        ((lines[0] |> Seq.map (fun _ -> false) |> Seq.toArray, 0), lines)
        ||> Array.fold outputBeamsAndNumSplits
        |> snd

module PartTwo =

    let numWorldsOut (numWorldsIn: int64 array) (line: string) = [|
        for i in 0 .. numWorldsIn.Length - 1 do
            match line[i] with
            | 'S' -> 1L
            | '.' ->
                let leftWorlds =
                    (numWorldsIn |> Seq.tryItem (i - 1), line |> Seq.tryItem (i - 1))
                    ||> Option.map2 (fun n c -> if c = '^' then n else 0L)
                    |> Option.defaultValue 0L

                let rightWorlds =
                    (numWorldsIn |> Seq.tryItem (i + 1), line |> Seq.tryItem (i + 1))
                    ||> Option.map2 (fun n c -> if c = '^' then n else 0L)
                    |> Option.defaultValue 0L

                leftWorlds + numWorldsIn[i] + rightWorlds
            // Assumes that there aren't two splitters next to each other, which is true for sample and my input.
            | '^' -> 0L
            | _ -> failwith "Bug"
    |]

    let solve (lines: string array) =
        (lines[0] |> Seq.map (fun _ -> 0L) |> Seq.toArray, lines)
        ||> Array.fold numWorldsOut
        |> Array.sum

module Test =

    open Expecto
    open Swensen.Unquote

    let all =
        testList "Day07" [
            let sampleInput = [|
                ".......S......."
                "..............."
                ".......^......."
                "..............."
                "......^.^......"
                "..............."
                ".....^.^.^....."
                "..............."
                "....^.^...^...."
                "..............."
                "...^.^...^.^..."
                "..............."
                "..^...^.....^.."
                "..............."
                ".^.^.^.^.^...^."
                "..............."
            |]

            testList "PartOne" [
                testCase "solve works with sample input" (fun _ -> test <@ PartOne.solve sampleInput = 21 @>)
            ]

            testList "PartTwo" [
                testCase "solve works with sample input" (fun _ -> test <@ PartTwo.solve sampleInput = 40 @>)
            ]
        ]

let dayFns = {
    Tests = Test.all
    UtilTests = []
    PartOne = PartOne.solve >> int64
    PartTwo = PartTwo.solve >> int64
}
