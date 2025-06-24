module Day24

open System.Collections.Generic

type GateKind =
    | AND
    | OR
    | XOR

type Gate = { Kind: GateKind; I1: string; I2: string; O: string }

let parseGate (str: string) =
    let parts = str.Split ' '

    {
        Kind =
            match parts[1] with
            | "AND" -> AND
            | "OR" -> OR
            | "XOR" -> XOR
            | s -> failwith $"Invalid gate kind %s{s}"
        I1 = parts[0]
        I2 = parts[2]
        O = parts[4]
    }

let parse (lines: string array) =
    lines
    |> fun ls -> System.String.Join('\n', ls)
    |> _.Split("\n\n")
    |> fun parts ->
        let values =
            parts[0].Split '\n'
            |> Array.map (fun s -> s[0..2], byte s[5] - 48uy)
            |> dict
            // Enable mutation.
            |> Dictionary

        let gates =
            parts[1].Split '\n'
            |> Array.map parseGate
            |> Array.map (fun g -> g.O, g)
            |> dict

        values, gates

/// Given gates and initial values, returns the z bits from least to most significant.
/// Also updates values to store all wire values.
let zBits (values: Dictionary<string, byte>) (gates: IDictionary<string, Gate>) =
    let rec getValue wire =
        match values.TryGet wire with
        | Some v -> v
        | None ->
            let g = gates[wire]

            let v =
                match g.Kind, getValue g.I1, getValue g.I2 with
                | AND, i1, i2 -> i1 &&& i2
                | OR, i1, i2 -> i1 ||| i2
                | XOR, i1, i2 -> i1 ^^^ i2

            values[wire] <- v
            v

    gates.Keys
    |> Seq.filter (fun g -> g.StartsWith 'z')
    |> Seq.sort
    |> Seq.toArray
    |> Array.map getValue

/// Assumes bits are least to most significant.
let toInt64 bits =
    ((0L, 1L), bits)
    ||> Array.fold (fun (acc, placeValue) zv -> acc + placeValue * int64 zv, placeValue * 2L)
    |> fst

module PartOne =

    let solve (lines: string array) = lines |> parse ||> zBits |> toInt64

module PartTwo =

    open FsToolkit.ErrorHandling

    /// The indexes of the bits that make up x and y, from least to most significant.
    let places = [| 0..44 |]

    /// Returns the first i for which x = 2^(i+1) - 1 and y = 1 does not give z = 2^(i+1), and the corresponding z.
    let firstIncorrectAddition gates =
        places
        |> Seq.map (fun i ->
            let vs =
                places
                |> Array.collect (fun j -> [|
                    $"x%02i{j}", if j <= i then 1uy else 0uy
                    $"y%02i{j}", if j = 0 then 1uy else 0uy
                |])
                |> dict
                |> Dictionary

            let zs = zBits vs gates

            let isCorrect =
                zs
                |> Array.indexed
                |> Array.forall (fun (j, b) -> b = if j = i + 1 then 1uy else 0uy)

            isCorrect, (i, zs))
        |> Seq.tryFind (not << fst)
        |> Option.map snd

    let test gates =
        gates
        |> firstIncorrectAddition
        |> Option.map (fun (i, zs) -> printfn $"Error in %i{i}th add; got 0b%B{toInt64 zs}.")
        |> Option.defaultWith (fun _ -> printfn "All tested additions give correct answer!")

    let swap o1 o2 (gates: IDictionary<string, Gate>) =
        let copy = Dictionary gates
        copy[o1] <- { gates[o2] with O = o1 }
        copy[o2] <- { gates[o1] with O = o2 }

        copy

    /// Identifies places where the circuit deviates from a ripple carry adder.
    /// See https://en.wikipedia.org/wiki/Adder_(electronics)#Ripple-carry_adder.
    let analyse (gates: IDictionary<string, Gate>) = validation {

        let placesList = places |> Array.toList
        let xorGates = gates.Values |> Seq.filter (_.Kind >> (=) XOR) |> Seq.toArray
        let andGates = gates.Values |> Seq.filter (_.Kind >> (=) AND) |> Seq.toArray
        let orGates = gates.Values |> Seq.filter (_.Kind >> (=) OR) |> Seq.toArray

        let hasInput i g = g.I1 = i || g.I2 = i
        let hasInputs i1 i2 g = hasInput i1 g && hasInput i2 g

        let optionToResult e o =
            o |> Option.map Ok |> Option.defaultValue (Error e)

        let! inputXors =
            placesList
            |> List.traverseResultA (fun i ->
                xorGates
                |> Array.tryFind (hasInputs $"x%02i{i}" $"y%02i{i}")
                |> optionToResult $"xor_%02i{i} is missing")

        let! inputAnds =
            placesList
            |> List.traverseResultA (fun i ->
                andGates
                |> Array.tryFind (hasInputs $"x%02i{i}" $"y%02i{i}")
                |> optionToResult $"and_%02i{i} is missing")

        // First bit is half-adder.
        let! out0 =
            let g = inputXors[0]

            if g.O = "z00" then
                Ok g
            else
                Error $"z00 is not output of x00 XOR y00"

        and! outsPlus =
            placesList
            // First bit is half-adder.
            |> List.skip 1
            |> List.traverseResultA (fun i ->
                xorGates
                |> Array.tryFind (hasInput inputXors[i].O)
                |> optionToResult $"out_%02i{i} is missing"
                |> Result.bind (fun g ->
                    if g.O = $"z%02i{i}" then
                        Ok g
                    else
                        Error $"out_%02i{i} has output %s{g.O}"))

        // First bit is half-adder.
        and! carriesPlus =
            placesList
            // First bit is half-adder.
            |> List.skip 1
            |> List.traverseResultA (fun i ->
                orGates
                |> Array.tryFind (hasInput inputAnds[i].O)
                |> optionToResult $"carry_%02i{i} is missing")

        let _carries = inputAnds[0] :: carriesPlus
        let _outs = out0 :: outsPlus

        return ()
    }

    let solve (lines: string array) =
        let gates = lines |> parse |> snd

        // Swaps identified from running analyse without those swaps and inspecting flagged parts of puzzle input.
        let swapped =
            gates
            |> swap "mkk" "z10"
            |> swap "qbw" "z14"
            |> swap "wcb" "z34"
            |> swap "wjb" "cvp"

        swapped |> analyse |> Result.defaultWith (List.iter (printfn "Error: %s"))
        test swapped

        let s =
            [| "mkk"; "z10"; "qbw"; "z14"; "wcb"; "z34"; "wjb"; "cvp" |]
            |> Array.sort
            |> fun xs -> System.String.Join(',', xs)

        printfn $"Day 24 part two answer is '%s{s}'"

        -1

module Test =

    open Expecto
    open Swensen.Unquote

    let all =
        testList "Day24" [
            let sampleInput = [|
                "x00: 1"
                "x01: 0"
                "x02: 1"
                "x03: 1"
                "x04: 0"
                "y00: 1"
                "y01: 1"
                "y02: 1"
                "y03: 1"
                "y04: 1"
                ""
                "ntg XOR fgs -> mjb"
                "y02 OR x01 -> tnw"
                "kwq OR kpj -> z05"
                "x00 OR x03 -> fst"
                "tgd XOR rvg -> z01"
                "vdt OR tnw -> bfw"
                "bfw AND frj -> z10"
                "ffh OR nrd -> bqk"
                "y00 AND y03 -> djm"
                "y03 OR y00 -> psh"
                "bqk OR frj -> z08"
                "tnw OR fst -> frj"
                "gnj AND tgd -> z11"
                "bfw XOR mjb -> z00"
                "x03 OR x00 -> vdt"
                "gnj AND wpb -> z02"
                "x04 AND y00 -> kjc"
                "djm OR pbm -> qhw"
                "nrd AND vdt -> hwm"
                "kjc AND fst -> rvg"
                "y04 OR y02 -> fgs"
                "y01 AND x02 -> pbm"
                "ntg OR kjc -> kwq"
                "psh XOR fgs -> tgd"
                "qhw XOR tgd -> z09"
                "pbm OR djm -> kpj"
                "x03 XOR y03 -> ffh"
                "x00 XOR y04 -> ntg"
                "bfw OR bqk -> z06"
                "nrd XOR fgs -> wpb"
                "frj XOR qhw -> z04"
                "bqk OR frj -> z07"
                "y03 OR x01 -> nrd"
                "hwm AND bqk -> z03"
                "tgd XOR rvg -> z12"
                "tnw OR pbm -> gnj"
            |]

            testList "PartOne" [
                testCase "solve works with sample input" (fun _ -> test <@ PartOne.solve sampleInput = 2024 @>)
            ]
        ]

let dayFns = {
    Tests = Test.all
    UtilTests = []
    PartOne = PartOne.solve >> int64
    PartTwo = PartTwo.solve >> int64
}
