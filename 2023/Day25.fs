module Day25

open System.Collections.Generic

[<AutoOpen>]
module Util =

    module WeightedGraph =

        /// Make a new weighted graph by condensing the vertices to a single vertex, maintaining the number of edges
        /// from the condensed vertex to other vertices. The output (and input) graphs can have multiple edges between
        /// the same two vertices.
        // TODO: add tests.
        let condense (graph: IDictionary<'a, ('a * int) array>) (vertices: 'a array) newLabel =

            let newConnections =
                vertices
                |> Array.collect (fun v -> graph.[v])
                |> Array.groupBy fst
                |> Array.choose (fun (v, cs) ->
                    if vertices |> Array.contains v then None else Some (v, cs |> Array.sumBy snd))

            graph
            :> _ seq
            |> Seq.append [| KeyValuePair(newLabel, newConnections) |]
            |> Seq.toArray
            |> Array.choose (fun kvp ->
                if vertices |> Array.contains kvp.Key then
                    None
                else
                    let toRemove, others = kvp.Value |> Array.partition (fun (v, _) -> vertices |> Array.contains v)
                    if not (toRemove |> Array.isEmpty) then
                        Some (kvp.Key, [| newLabel, toRemove |> Array.sumBy snd; yield! others |])
                    else
                        Some (kvp.Key, others))
            |> dict

        module StoerWagnerInternals =

            type MinCutPhaseData<'a> = {
                S: 'a
                T: 'a
                CutOfThePhase: 'a array * 'a array
                Weight: int
            }

            /// The minimum cut phase of the Stoer-Wagner algorithm. Note: I haven't used the method that the authors
            /// refer to for finding the vertex that is most connected to the already-added vertices; I haven't done
            /// a complexity analysis, but it's fast enough for now.
            let minimumCutPhase combineVertices (weightedGraph: IDictionary<'a, ('a * int) array>) =

                let rec inner (g: IDictionary<'a, ('a * int) array>) a (remaining: 'a array) lastAdded =

                    let graphSize = weightedGraph.Count.ToString("D4")
                    let remainingSize = remaining.Length.ToString("D4")
                    printfn $"Graph size: %s{graphSize}; remaining: %s{remainingSize}"

                    if remaining.Length > 1 then
                        let mostConnectedToA = g.[a] |> Array.maxBy snd |> fst
                        let toCombine = [| a; mostConnectedToA |]
                        let newLabel = combineVertices toCombine
                        let condensed = condense g toCombine newLabel
                        inner condensed newLabel (remaining |> Array.except [| mostConnectedToA |]) mostConnectedToA

                    else
                        remaining, lastAdded

                let vertices = weightedGraph.Keys |> Seq.toArray
                let remaining, lastAdded =
                    inner weightedGraph (vertices |> Array.head) (vertices |> Array.skip 1) (vertices |> Array.head)

                match remaining with
                | [| t |] ->
                    {
                        S = lastAdded
                        T = t
                        CutOfThePhase = ([| t |], vertices |> Array.except [| t |])
                        Weight = weightedGraph.[t] |> Array.sumBy snd
                    }
                | _ -> failwith "Bug"

        /// Finds the min cut of a weighted graph using the Stoer-Wagner algorithm.
        /// See https://www.wikiwand.com/en/Stoer%E2%80%93Wagner_algorithm.
        /// Returns early if an acceptable min cut if found.
        // TODO: add tests.
        let stoerWagnerMinCut isAcceptableCut combineVertices (weightedGraph: IDictionary<'a, ('a * int) array>) =

            let rec inner (g: IDictionary<'a, ('a * int) array>) minCutSoFar =

                if g.Count > 1 && not (minCutSoFar |> Option.map isAcceptableCut |> Option.defaultValue false) then

                    let phaseData = StoerWagnerInternals.minimumCutPhase combineVertices g

                    let newMinCut =
                        match minCutSoFar with
                        | Some (cut, weight) when phaseData.Weight >= weight -> Some (cut, weight)
                        | Some _
                        | None -> Some (phaseData.CutOfThePhase, phaseData.Weight)

                    let toCombine = [| phaseData.S; phaseData.T |]
                    let newG = condense g toCombine (combineVertices toCombine)

                    inner newG newMinCut

                else
                    g, minCutSoFar

            inner weightedGraph None |> snd

    [<RequireQualifiedAccess>]
    module Array =

        let private equalLength xs ys =
            if Array.length xs = Array.length ys then
                true
            else
                failwith $"%A{xs} is different length to %A{ys}"

        let private sameElements isSameAs xs ys =
            (Array.sort xs, Array.sort ys)
            ||> Array.forall2 (fun x y -> if x |> isSameAs y then true else failwith $"%A{x} <> %A{y}")

        let containsEquivalentElements isSameAs xs ys =
            equalLength xs ys && sameElements isSameAs xs ys

        let containsSameElements<'a when 'a : comparison> (xs: 'a array) (ys: 'a array) =
            containsEquivalentElements (=) xs ys

/// Converts lines into a graph of connected components (nodes) and wires (edges), represented as a dictionary from a
/// node's label to the labels of the connected nodes.
let parse lines =
    lines
    |> Array.collect (fun (line: string) ->
        let parts = line.Split(": ")
        let a = parts.[0]
        let bs = parts.[1].Split(" ")
        [| for b in bs do a, b; b, a |])
    |> Array.groupBy fst
    // Assumes modules are only connected one-to-one, which is true of the input.
    |> Array.map (fun (node, edges) -> node, edges |> Array.map (fun edge -> snd edge, 1))
    |> dict

module PartOne =

    let solve lines =
        lines
        |> parse
        |> WeightedGraph.stoerWagnerMinCut (snd >> (=) 3) (fun (ns: string array) -> System.String.Join(";", ns))
        |> Option.defaultWith (fun _ -> failwith "Invalid input: no min cut")
        |> fun ((part1, part2), _) ->
            let part1Size = part1 |> Array.collect (fun s -> s.Split(';')) |> Array.length
            let part2Size = part2 |> Array.collect (fun s -> s.Split(';')) |> Array.length
            part1Size * part2Size

module Test =

    open Swensen.Unquote
    open Expecto

    let all = testList "Day25" [

        testList "Sample input" [

            let sampleInput = [|
                "jqt: rhn xhk nvd"
                "rsh: frs pzl lsr"
                "xhk: hfx"
                "cmg: qnr nvd lhk bvb"
                "rhn: xhk bvb hfx"
                "bvb: xhk hfx"
                "pzl: lsr hfx nvd"
                "qnr: nvd"
                "ntq: jqt hfx bvb xhk"
                "nvd: lhk"
                "lsr: lhk"
                "rzs: qnr cmg lsr rsh"
                "frs: qnr lhk lsr"
            |]

            let nodes = [|
                "jqt"
                "rsh"
                "xhk"
                "cmg"
                "rhn"
                "bvb"
                "pzl"
                "qnr"
                "ntq"
                "nvd"
                "lsr"
                "rzs"
                "frs"
                "hfx"
                "lhk"
            |]

            let sampleGraph =
                [|
                    "jqt", [| ("rhn", 1); ("xhk", 1); ("nvd", 1); ("ntq", 1) |]
                    "rsh", [| ("frs", 1); ("pzl", 1); ("lsr", 1); ("rzs", 1) |]
                    "xhk", [| ("hfx", 1); ("jqt", 1); ("rhn", 1); ("bvb", 1); ("ntq", 1) |]
                    "cmg", [| ("qnr", 1); ("nvd", 1); ("lhk", 1); ("bvb", 1); ("rzs", 1) |]
                    "rhn", [| ("xhk", 1); ("bvb", 1); ("hfx", 1); ("jqt", 1) |]
                    "bvb", [| ("xhk", 1); ("hfx", 1); ("cmg", 1); ("rhn", 1); ("ntq", 1) |]
                    "pzl", [| ("lsr", 1); ("hfx", 1); ("nvd", 1); ("rsh", 1) |]
                    "qnr", [| ("nvd", 1); ("cmg", 1); ("rzs", 1); ("frs", 1) |]
                    "ntq", [| ("jqt", 1); ("hfx", 1); ("bvb", 1); ("xhk", 1) |]
                    "nvd", [| ("lhk", 1); ("jqt", 1); ("cmg", 1); ("pzl", 1); ("qnr", 1) |]
                    "lsr", [| ("lhk", 1); ("rsh", 1); ("pzl", 1); ("rzs", 1); ("frs", 1) |]
                    "rzs", [| ("qnr", 1); ("cmg", 1); ("lsr", 1); ("rsh", 1) |]
                    "frs", [| ("qnr", 1); ("lhk", 1); ("lsr", 1); ("rsh", 1) |]
                    "hfx", [| ("xhk", 1); ("rhn", 1); ("bvb", 1); ("pzl", 1); ("ntq", 1) |]
                    "lhk", [| ("cmg", 1); ("nvd", 1); ("lsr", 1); ("frs", 1) |]
                |]
                |> dict

            test "Parse returns expected Map" {
                let actual = parse sampleInput

                let actualKeys = actual.Keys |> Seq.toArray
                Assertions.test
                    <@
                       actualKeys |> Array.containsSameElements nodes
                        &&
                            actualKeys
                            |> Array.forall (fun key -> actual.[key] |> Array.containsSameElements sampleGraph.[key])
                    @>
            }

            testList "WeightedGraph.condense" [

                let inputResultsInExpectedOutput (nodes: string array) expected =
                    test $"given %A{nodes} returns expected graph" {
                        let actual =
                            let newLabel = System.String.Join(";", nodes)
                            WeightedGraph.condense sampleGraph nodes newLabel
                            |> Seq.toArray
                            |> Array.map (fun kvp -> kvp.Key, kvp.Value)

                        let pairsAreSame (x: string, xs: (string * int) array) (y, ys) =
                            x = y && xs |> Array.containsSameElements ys
                        <@ (actual :> _ seq |> Seq.toArray) |> Array.containsEquivalentElements pairsAreSame expected @>
                        |> Assertions.test
                    }

                inputResultsInExpectedOutput
                    [| "xhk"; "rhn" |]
                    [|
                        "jqt", [| ("xhk;rhn", 2); ("nvd", 1); ("ntq", 1) |]
                        "rsh", [| ("frs", 1); ("pzl", 1); ("lsr", 1); ("rzs", 1) |]
                        "xhk;rhn", [| ("hfx", 2); ("jqt", 2); ("bvb", 2); ("ntq", 1) |]
                        "cmg", [| ("qnr", 1); ("nvd", 1); ("lhk", 1); ("bvb", 1); ("rzs", 1) |]
                        "bvb", [| ("xhk;rhn", 2); ("hfx", 1); ("cmg", 1); ("ntq", 1) |]
                        "pzl", [| ("lsr", 1); ("hfx", 1); ("nvd", 1); ("rsh", 1) |]
                        "qnr", [| ("nvd", 1); ("cmg", 1); ("rzs", 1); ("frs", 1) |]
                        "ntq", [| ("jqt", 1); ("hfx", 1); ("bvb", 1); ("xhk;rhn", 1) |]
                        "nvd", [| ("lhk", 1); ("jqt", 1); ("cmg", 1); ("pzl", 1); ("qnr", 1) |]
                        "lsr", [| ("lhk", 1); ("rsh", 1); ("pzl", 1); ("rzs", 1); ("frs", 1) |]
                        "rzs", [| ("qnr", 1); ("cmg", 1); ("lsr", 1); ("rsh", 1) |]
                        "frs", [| ("qnr", 1); ("lhk", 1); ("lsr", 1); ("rsh", 1) |]
                        "hfx", [| ("xhk;rhn", 2); ("bvb", 1); ("pzl", 1); ("ntq", 1) |]
                        "lhk", [| ("cmg", 1); ("nvd", 1); ("lsr", 1); ("frs", 1) |]
                    |]

                inputResultsInExpectedOutput
                    [| "xhk"; "rhn"; "hfx" |]
                    [|
                        "jqt", [| ("xhk;rhn;hfx", 2); ("nvd", 1); ("ntq", 1) |]
                        "rsh", [| ("frs", 1); ("pzl", 1); ("lsr", 1); ("rzs", 1) |]
                        "xhk;rhn;hfx", [| ("jqt", 2); ("bvb", 3); ("ntq", 2); ("pzl", 1) |]
                        "cmg", [| ("qnr", 1); ("nvd", 1); ("lhk", 1); ("bvb", 1); ("rzs", 1) |]
                        "bvb", [| ("xhk;rhn;hfx", 3); ("cmg", 1); ("ntq", 1) |]
                        "pzl", [| ("lsr", 1); ("xhk;rhn;hfx", 1); ("nvd", 1); ("rsh", 1) |]
                        "qnr", [| ("nvd", 1); ("cmg", 1); ("rzs", 1); ("frs", 1) |]
                        "ntq", [| ("jqt", 1); ("xhk;rhn;hfx", 2); ("bvb", 1) |]
                        "nvd", [| ("lhk", 1); ("jqt", 1); ("cmg", 1); ("pzl", 1); ("qnr", 1) |]
                        "lsr", [| ("lhk", 1); ("rsh", 1); ("pzl", 1); ("rzs", 1); ("frs", 1) |]
                        "rzs", [| ("qnr", 1); ("cmg", 1); ("lsr", 1); ("rsh", 1) |]
                        "frs", [| ("qnr", 1); ("lhk", 1); ("lsr", 1); ("rsh", 1) |]
                        "lhk", [| ("cmg", 1); ("nvd", 1); ("lsr", 1); ("frs", 1) |]
                    |]

            ]

            test "PartOne.solve gives correct answer" {
                let actual = PartOne.solve sampleInput

                Assertions.test <@ actual = 54 @>
            }

        ]

    ]
