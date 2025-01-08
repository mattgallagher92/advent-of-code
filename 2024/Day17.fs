module Day17

type ProgramState = {
    Program: int array
    RegA: int
    RegB: int
    RegC: int
    InstrPointer: int
} with

    member this.Opcode = this.Program[this.InstrPointer]
    member this.LiteralOperand = this.Program[this.InstrPointer + 1]

    member this.ComboOperand =
        match this.Program[this.InstrPointer + 1] with
        | x when -1 < x && x < 4 -> x
        | 4 -> this.RegA
        | 5 -> this.RegB
        | 6 -> this.RegC
        | x -> failwith $"Invalid combo operand input %i{x}"

    member this.Halt = this.InstrPointer >= this.Program.Length

let (|RegisterA|RegisterB|RegisterC|) str =
    match
        str
        |> Regex.regexMatch "Register (\w): (\d+)"
        |> _.Groups
        |> Seq.toList
        |> List.map _.Value
    with
    | [ _; "A"; v ] -> RegisterA(int v)
    | [ _; "B"; v ] -> RegisterB(int v)
    | [ _; "C"; v ] -> RegisterC(int v)
    | _ -> failwith $"Invalid register line: %s{str}"

let (|Program|) str =
    match
        str
        |> Regex.regexMatch "Program: (.+)"
        |> _.Groups
        |> Seq.toList
        |> List.map _.Value
    with
    | [ _; s ] -> s.Split ',' |> Array.map int
    | _ -> failwith $"Invalid register line: %s{str}"

let parse (lines: string array) =
    let emptyLineIx = lines |> Array.findIndex System.String.IsNullOrEmpty

    match lines |> Array.splitAt emptyLineIx with
    | [| RegisterA a; RegisterB b; RegisterC c |], [| ""; Program p |] -> {
        Program = p
        RegA = a
        RegB = b
        RegC = c
        InstrPointer = 0
      }
    | _ -> failwith $"Invalid input %A{lines}"

let executeInstruction (state: ProgramState) =
    match state.Opcode with
    | 0 -> { state with RegA = state.RegA >>> state.ComboOperand }, None
    | 1 -> { state with RegB = state.RegB ^^^ state.LiteralOperand }, None
    | 2 -> { state with RegB = state.ComboOperand &&& 0b111 }, None
    | 3 ->
        match state.RegA with
        | 0 -> state, None
        | _ -> { state with InstrPointer = state.LiteralOperand - 2 }, None
    | 4 -> { state with RegB = state.RegB ^^^ state.RegC }, None
    | 5 -> state, Some(state.ComboOperand &&& 0b111)
    | 6 -> { state with RegB = state.RegA >>> state.ComboOperand }, None
    | 7 -> { state with RegC = state.RegA >>> state.ComboOperand }, None
    | x -> failwith $"Invalid opcode %i{x}"
    |> Pair.mapFst (fun state -> { state with InstrPointer = state.InstrPointer + 2 })

[<TailCall>]
let rec runInner (state: ProgramState) output =
    if state.Halt then
        output |> List.choose id |> List.rev
    else
        let newState, out = state |> executeInstruction
        runInner newState (out :: output)

let run state = runInner state []

module PartOne =

    let solveString (lines: string array) =
        lines |> parse |> run |> (fun o -> System.String.Join(',', o))

    let solve (lines: string array) =
        lines
        |> solveString
        |> fun s ->
            printfn $"Day 17 part one solution: %s{s}"
            -1

module PartTwo =

    let solve (lines: string array) = -1

module Test =

    open Expecto
    open Swensen.Unquote

    let all =
        testList "Day17" [
            let sampleInput = [|
                "Register A: 729"
                "Register B: 0"
                "Register C: 0"
                ""
                "Program: 0,1,5,4,3,0"
            |]

            testList "PartOne" [
                testCase "solve works with sample input" (fun _ ->
                    test <@ PartOne.solveString sampleInput = "4,6,3,5,6,3,5,2,1,0" @>)
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
