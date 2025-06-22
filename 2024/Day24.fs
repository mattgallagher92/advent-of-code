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

module PartOne =

    let solve (lines: string array) =
        let values, gates = lines |> parse

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

        let zValues =
            gates.Keys
            |> Seq.filter (fun g -> g.StartsWith 'z')
            // Least to most significant
            |> Seq.sort
            |> Seq.toArray
            |> Array.map getValue

        ((0L, 1L), zValues)
        ||> Array.fold (fun (acc, placeValue) zv -> acc + placeValue * int64 zv, placeValue * 2L)
        |> fst

module PartTwo =

    let solve (lines: string array) = -1

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

            testList "PartTwo" [
                testCase "solve works with sample input" (fun _ -> test <@ PartTwo.solve sampleInput = -1 @>)
            ]
        ]

let dayFns = {
    Tests = Test.all
    UtilTests = []
    PartOne = PartOne.solve >> int64
    PartTwo = PartTwo.solve >> int64
}
