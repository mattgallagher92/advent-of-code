module Day20

open System.Collections.Generic

type Pulse =
    | LowPulse
    | HighPulse

type FlipFlopModule =
    {
        IsOn: bool
    }

    member this.HandleInput =
        function
        | LowPulse ->
            let nowOn = not this.IsOn
            let output = if nowOn then HighPulse else LowPulse
            { IsOn = nowOn }, Some output
        | HighPulse  ->
            this, None


type ConjunctionModule =
    {
        InputPulses: Map<string, Pulse>
    }

    member this.HandleInput connectionLabel pulse =
        let newInputs = this.InputPulses |> Map.add connectionLabel pulse

        let output =
            if newInputs |> Map.forall (fun _ pulse -> pulse = HighPulse) then
                LowPulse
            else
                HighPulse

        { InputPulses = newInputs },  output

type Module =
    | FlipFlop of FlipFlopModule
    | Conjunction of ConjunctionModule
    | Broadcast

module Module =

    let newFlipFlop = FlipFlop { IsOn = false }

    let newConjunction inputConnectionLabels =
        Conjunction { InputPulses = inputConnectionLabels |> Seq.map (fun l -> l, LowPulse) |> Map }

    let handleInput outputLabel pulse =
        function
        | FlipFlop ff ->
            let newState, output = ff.HandleInput pulse
            FlipFlop newState, output
        | Conjunction c ->
            let newState, output = c.HandleInput outputLabel pulse
            Conjunction newState, Some output
        | Broadcast ->
            Broadcast, Some pulse

type Connection = {
    /// The label of the module whose output the connection attaches to.
    OutputLabel: string
    /// The label of the module whose input the connection attaches to.
    InputLabel: string
}

/// Given a configuration before a button press, returns the configuration after the pulses have stopped being
/// sent, and a list of sent pulses that were sent paired with the connections that they were sent on.
let handleButtonPress (configuration: Dictionary<string, Module * Connection array>) =

    let newConfig = Dictionary(configuration)

    let queue = Queue<Pulse * Connection>()

    let tryDequeue () =
        match queue.TryDequeue() with
        | true, item -> Some item
        | false, _ -> None

    queue.Enqueue(LowPulse, { OutputLabel = "button"; InputLabel = "broadcaster" })

    let signals =
        ()
        |> List.unfold (fun _ ->
            tryDequeue ()
            |> Option.map (fun item ->

                let pulse, { OutputLabel = outputLabel; InputLabel = inputLabel } = item

                newConfig.TryGet inputLabel
                |> Option.iter (fun (module', outputConnections) ->

                    let module'', output = module' |> Module.handleInput outputLabel pulse

                    newConfig.[inputLabel] <- (module'', outputConnections)

                    output
                    |> Option.iter (fun output ->
                        outputConnections |> Array.iter (fun conn -> queue.Enqueue(output, conn))))

                Some item, ()))
        |> List.choose id

    newConfig, signals

let parse (lines: string array) =

    // Turn into more structured data.
    let data =
        lines
        |> Array.map (fun line ->
            let parts = line.Split(" -> ")
            let typeSignifier, moduleLabel =
                match parts.[0].[0] with
                | '%' -> Some '%', parts.[0].[ 1 .. ]
                | '&' -> Some '&', parts.[0].[ 1 .. ]
                | _ -> None, parts.[0]
            let outputConnectionLabels = parts.[1].Split(", ")
            moduleLabel, typeSignifier, outputConnectionLabels)

    // First pass to build up dictionary from module label to input labels.
    let toInputLabels =
        data
        |> Array.collect (fun (moduleLabel, _, outputConnectionLabels) ->
            outputConnectionLabels |> Array.map (fun outputLabel -> outputLabel, moduleLabel))
        |> Array.groupBy fst
        |> Seq.map (fun (k, vs) -> k, vs |> Array.map snd)
        |> Map
        |> Dictionary

    // Second pass to create config using map.
    let config =
        data
        |> Array.map (fun (moduleLabel, typeSignifier, outputConnectionLabels) ->

            let module' =
                match typeSignifier with
                | Some '%' -> Module.newFlipFlop
                | Some '&' -> Module.newConjunction toInputLabels.[moduleLabel]
                | Some _ -> failwith "Bug"
                | None -> Broadcast

            let connections =
                outputConnectionLabels |> Array.map (fun l -> { OutputLabel = moduleLabel; InputLabel = l })

            moduleLabel, (module', connections))
        |> Map
        |> Dictionary

    config, toInputLabels

module PartOne =

    let handleNButtonPresses n config =

        let rec inner remaining config signals =

            if remaining = 0 then
                config, signals |> Seq.toArray
            else
                let newConfig, newSignals = config |> handleButtonPress
                inner (remaining - 1) newConfig (Seq.append signals newSignals)

        inner n config Seq.empty

    let solve lines =

        let pulses = lines |> parse |> fst |> handleNButtonPresses 1000 |> snd |> Array.map fst

        pulses
        |> Seq.countBy id
        |> Seq.map snd
        |> Seq.reduce (*)

module PartTwo =

    /// Returns the first index of a button press resulting in a low pulse being input to the module with the given
    /// label, provided that the first ten such indices are all divisible by the first.
    let lowPulseButtonPressIndexPatterns config label =

        let rec inner lowPulseIndices buttonPressCount config =

            let newConfig, signals = config |> handleButtonPress
            let newCount = buttonPressCount + 1

            let receivedLowPulse =
                signals
                |> List.exists (fun (pulse, connection) -> pulse = LowPulse && connection.InputLabel = label)

            match receivedLowPulse, lowPulseIndices |> List.length with
            | true, 9 ->
                let inOrder = lowPulseIndices |> List.rev
                let smallest = inOrder |> List.head
                if inOrder |> List.forall (fun i -> i % smallest = 0) then
                    smallest
                else
                    failwith $"Button press indices resulting in low pulses to %s{label} aren't cyclic: %A{inOrder}"
            | true, _ ->
                inner (newCount :: lowPulseIndices) newCount newConfig
            | false, _ ->
                inner lowPulseIndices newCount newConfig

        inner [] 0 config

    // Note: this solution isn't generic - it makes various assumptions about the input that are true for the input
    // that I have (and I suspect all inputs). If those assumptions do not hold, this function will fail with an
    // exception.
    let solve lines =

        let config, toInputLabels = lines |> parse

        let outputtingToRxLabel =
            match toInputLabels.["rx"] with
            | [| label |] -> label
            | labels -> failwith $"%A{labels} all output to rx, breaking assumption that there is only one."

        let outputtingToRxPredecessor =
            match config.[outputtingToRxLabel] with
            | Conjunction module', _ -> module'.InputPulses.Keys
            | _ -> failwith "The module outputting to rx is assumed to be a conjunction module."

        outputtingToRxPredecessor
        |> Seq.map (lowPulseButtonPressIndexPatterns config)
        |> Seq.map uint64
        |> Seq.reduce Math.lcm

