module Day8

open System

type Direction =
    | Left
    | Right

type Node = {
    Label: char * char * char
    LeftLabel: char * char * char
    RightLabel: char * char * char
}

let parse lines =

    let directions =
        lines
        |> Array.item 0
        |> Seq.map (function
            | 'L' -> Left
            | 'R' -> Right
            | c -> failwith $"Invalid direction $c{c}")

    let nodes =
        lines
        |> Seq.skip 2
        |> Seq.map (fun line ->
            match line |> Seq.toList with
            | c1 :: c2 :: c3 :: ' ' :: '=' :: ' ' :: '(' :: l1 :: l2 :: l3 :: ',' :: ' ' :: r1 :: r2 :: r3 :: [ ')' ] ->
                {
                    Label = c1, c2, c3
                    LeftLabel = l1, l2, l3
                    RightLabel = r1, r2, r3
                }
            | _ -> failwith $"Invalid line: %s{line}")

    directions |> Seq.toArray, nodes |> Seq.toArray

module PartOne =

    let solve lines =

        let directions, nodes = parse lines

        let steps =
            let lookup = nodes |> Seq.map (fun n -> n.Label, n) |> dict
            let direction i = Array.item (i % directions.Length) directions
            Array.unfold
                (fun (i, node) ->
                    match node.Label, direction i with
                    | ('Z', 'Z', 'Z'), _ -> None
                    | _, Left -> let n = lookup.Item node.LeftLabel in Some (n, (i + 1, n))
                    | _, Right -> let n = lookup.Item node.RightLabel in Some (n, (i + 1, n)))
                (0, lookup.Item ('A', 'A', 'A'))

        steps.Length

module PartTwo =

    type PathSignature = {
        /// The number of steps before the cycle starts.
        PreCycleLength: int
        /// The indices of steps before the cycle that correspond to nodes whose label ends in Z.
        PreCycleZIndices: int array
        /// The number of steps in the cycle.
        CycleLength: int
        /// The indices, relative to the cycle start, of steps within the cycle that correspond to nodes whose label
        /// ends in Z.
        CycleZIndices: int array
    }

    let solve lines =

        let directions, nodes = parse lines
        let nodeLookup = nodes |> Seq.map (fun n -> n.Label, n) |> dict

        let startNodes =
            nodes
            |> Array.filter (fun n -> match n.Label with _, _, 'A' -> true | _ -> false)

        /// For a given node, returns the nodes visited by starting there and following directions in order. Returns
        /// the last such node separately.
        let journeyLookup =
            let journey startNode =
                let nodesVisited =
                    directions
                    |> Array.scan
                        (fun n d ->
                            match d with
                            | Left -> nodeLookup.Item n.LeftLabel
                            | Right -> nodeLookup.Item n.RightLabel)
                        startNode
                nodesVisited.[0 .. (nodesVisited.Length - 2)], nodesVisited |> Array.last

            nodes
            |> Array.map (fun n -> n, journey n)
            |> dict

        // Keep repeating a journey (following all directions through once) until we end at a node that was the
        // start of a previous journey. Returns the nodes at the start of those journeys, and the node that is
        // repeated first.
        let journeyStartsUntilFirstRepeat startNode =

            let starts =
                ([| startNode |], Seq.initInfinite ignore)
                ||> Seq.scan
                    (fun previousStarts ()  ->
                        if Seq.containsRepeats previousStarts then
                            previousStarts
                        else
                            let previous = previousStarts |> Array.last
                            let _, nextStart = journeyLookup.Item previous
                            Array.append previousStarts [| nextStart |])
                |> Seq.skipWhile (not << Seq.containsRepeats)
                |> Seq.head

            starts |> Array.removeAt (starts.Length - 1), starts |> Array.last

        /// Returns the steps that are before a cycle starts, and a cycle of steps starting immediately afterwards
        /// that repeats forever.
        let decomposeFrom startNode =

            let journeyStarts, cycleStart = journeyStartsUntilFirstRepeat startNode

            let cycleStartIndex = journeyStarts |> Array.findIndex ((=) cycleStart)
            let preCycle =
                journeyStarts
                |> Array.take cycleStartIndex
                |> Array.collect (fun n -> journeyLookup.Item n |> fst)
            let cycle =
                journeyStarts
                |> Array.skip cycleStartIndex
                |> Array.collect (fun n -> journeyLookup.Item n |> fst)

            preCycle, cycle

        let pathSignatures =
            let isZNode n = match n.Label with _, _, 'Z' -> true | _ -> false

            startNodes
            |> Array.map (fun startNode ->
                let preCycle, cycle = decomposeFrom startNode
                {
                    PreCycleLength = preCycle.Length
                    PreCycleZIndices =
                        preCycle
                        |> Array.mapi (fun i n -> i, isZNode n)
                        |> Array.filter snd
                        |> Array.map fst
                    CycleLength = cycle.Length
                    CycleZIndices =
                        cycle
                        |> Array.mapi (fun i n -> i, isZNode n)
                        |> Array.filter snd
                        |> Array.map fst
                })

        // TODO: implement in generic way.
        // This is a hack that makes use of some observations I've made about the particular input I have. It's not
        // sufficiently generic to handle every possible input of this type.
        //
        // The observations that I've made use of are:
        // - There are no pre-cycle z-indices
        // - The pre-cycles are all the same length
        // - There is only one cycle z-index, which is pre-cycle length away from the end of the cycle.
        //
        // As a result, any step index that is a z-index of a path must be divisible by that path signature's cycle
        // length. So a step index that is a z-index of all paths is divisible by all cycle lengths. The first such
        // step index is the least common multiple of the cycle lengths.
        pathSignatures
        |> Array.map (fun i -> i.CycleLength |> Convert.ToUInt64)
        |> Array.reduce Math.lcm

