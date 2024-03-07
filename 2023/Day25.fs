module Day25

open System.Collections.Generic

[<AutoOpen>]
module Util =

    [<RequireQualifiedAccess>]
    module Array =

        /// Returns all quadruples (a, b, c, d) where the indexes of a, b, c and d in xs are strictly increasing.
        let allDistinctQuadruples (xs: 'a array) = [|
            for i in 0 .. xs.Length - 1 do
                for j in (i + 1) .. xs.Length - 1 do
                    for k in (j + 1) .. xs.Length - 1 do
                        for l in (k + 1) .. xs.Length - 1 do
                            xs.[i], xs.[j], xs.[k], xs.[l]
        |]

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

    let private cache = Dictionary<IDictionary<string, (string * int) array> * string * int, (string * string) array array>()

    let rec pathsOfLength (graph: IDictionary<string, (string * int) array>) startNode length : (string * string) array array =
        let neighbours = graph.[startNode]
        if length = 1 then
            neighbours |> Array.collect (fun (n, c) -> Array.replicate c [| (startNode, n) |])
        else
            neighbours
            |> Array.collect (fun (n, c) ->
                pathsOfLength graph n (length - 1)
                |> Array.collect (fun p -> Array.replicate c [| (startNode, n); yield! p |]))

    /// Finds nodes that are connected by more than three independent paths (paths sharing no edges) of length up to
    /// maxPathLength in graph starting at startNode. Such nodes must be in the same partition as startNode (there must
    /// still be a connection even after cutting three wires), but this is not an exhaustive list of all nodes that will
    /// be in the same partition.
    let findNodesInSamePartition maxPathLength (graph: IDictionary<string, (string * int) array>) startNode =

        let cycleFreePathsUpToMaxLength =
            [| 1 .. maxPathLength |]
            |> Array.collect (pathsOfLength graph startNode)
            // For each path with a repeated vertex, there is a shorter path without repeated vertices that goes to the
            // same destination that the longer path shares edges with (remove the cycle).
            |> Array.filter (fun p ->
                // If there are no repeated vertices then each edge adds one vertex, so the number of distinct vertices
                // should be the number of edges plus one (there are two edges if there is one edge).
                p |> Array.collect (fun (v1, v2) -> [| v1; v2 |]) |> Set |> Set.count = p.Length + 1)

        let neighbourEdgeCounts =
            graph.Keys
            |> Seq.collect (fun v -> graph.[v] |> Array.map (fun (v', c) -> (v, v'), c))
            |> dict

        // TODO: feels wildly inefficient!
        // Find the destination nodes that have more than 3 independent paths (with no common edges) from the start node
        // to them. Those nodes must be in the same partition (connected group).
        cycleFreePathsUpToMaxLength
        |> Array.groupBy (Array.last >> snd)
        |> Array.filter (fun (_dest, pathsToDest) ->
            let intersectedVertices path = Set.union (Set (path |> Array.map fst)) (Set (path |> Array.map snd))
            pathsToDest
            |> Array.map (fun p -> p, intersectedVertices p)
            |> Array.allDistinctQuadruples
            |> Array.exists (fun ((p1, vs1), (p2, vs2), (p3, vs3), (p4, vs4)) ->
                let edges = Array.concat [| p1; p2; p3; p4 |]
                let vertices = Set.unionMany [| vs1; vs2; vs3; vs4 |]
                vertices
                |> Set.forall (fun v ->
                    edges
                    |> Array.choose (fun e ->
                        match e with
                        | a, b when a = v -> Some b
                        | a, b when b = v -> Some a
                        | _ -> None)
                    |> Array.countBy id
                    |> Array.forall (fun (v', m) -> m <= neighbourEdgeCounts.[v, v']))))
        |> Array.map fst
        // startNode is in the same partition, but will not be found by the above because we're restricting to
        // cycle-free paths.
        |> Array.append [| startNode |]

    /// Make a new (multi-) graph by condensing the nodes to a single node, maintaining the number of edges from the
    /// condensed node to other nodes. The output (and input) graphs can have multiple edges between the same two nodes.
    let condense (graph: IDictionary<string, (string * int) array>) (nodes: string array) newLabel =

        let d = Dictionary(graph)

        for node in nodes do
            d.Remove node |> ignore

        for n in d.Keys do
            d.[n] <-
                d.[n]
                |> Array.partition (fun (n, _) -> nodes |> Array.contains n)
                |> fun (toRemove, others) ->
                    [|
                        if not (toRemove |> Array.isEmpty) then newLabel, toRemove |> Array.sumBy snd
                        yield! others
                    |]

        let newConnections =
            nodes
            |> Array.collect (fun n -> graph.[n])
            |> Array.groupBy fst
            |> Array.choose (fun (n, cs) -> if nodes |> Array.contains n then None else Some (n, cs |> Array.sumBy snd))
        d.Add(newLabel, newConnections)

        d

    /// Repeatedly condenses nodes with more than three connection to the given node into that node until there are no
    /// such nodes.
    let rec shrink (graph: IDictionary<string, (string * int) array>) node =

        let toShrink =
            graph.[node]
            |> Array.filter (fun (_, count) -> count > 3)
            |> Array.map fst

        if toShrink |> Array.isEmpty then
            graph
        else
            let toShrink = [| node; yield! toShrink |]
            let newLabel = System.String.Join(";", toShrink)
            shrink (condense graph toShrink newLabel) newLabel

    let solve lines =

        // Make a graph of the connected components (nodes) and wires (edges).
        let graph = lines |> parse

        let rec inner (graph: IDictionary<string, (string * int) array>) lastCondensed attemptCount =

            printfn $"%A{graph |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Seq.toArray}"

            if graph.Keys.Count = 2 then
                graph

            elif attemptCount = graph.Keys.Count - 1 then
                let printable = graph |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Seq.toArray
                failwith $"Cannot reduce further but still more than 2 parts. Graph: %A{printable}"

            else

                let startNode =
                    graph.Keys |> Seq.sort |> Seq.filter (fun k -> Some k <> lastCondensed) |> Seq.item attemptCount
                // Uneducated guess: 5 steps will finish relatively quickly.
                let inSamePartition = findNodesInSamePartition 5 graph startNode

                if inSamePartition.Length = 1 then
                    inner graph lastCondensed (attemptCount + 1)

                else
                    // Make a new (multi-) graph where the partition is reduced to a single node. Ensure that any nodes
                    // that were connected to multiple nodes in the partition now have multiple edges connecting it to
                    // the partition.
                    let newLabel = System.String.Join(";", inSamePartition)
                    condense graph inSamePartition newLabel
                    |> fun condensed -> shrink condensed newLabel
                    |> fun shrunk -> inner shrunk (Some newLabel) 0

        // 6. Repeat steps 1-5, with a node outside of the new partition, until some stop condition. Perhaps 10
        // consecutive iterations without reducing the number of partitions or reduced to two partitions.
        let partitioned = inner graph None 0

        // 7. If more than 2 partitions, brute force check. One possible approach is to split the nodes into two groups
        // and check whether there are exactly 3 inter-partition edges. There are 2^n possible partitions, so might want
        // to throw if n is too large.
        match partitioned |> Seq.toArray with
        | [| kv1; kv2 |] -> kv1.Key.Split(';').Length * kv2.Key.Split(';').Length
        | _ -> failwith "bug"

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

            test "PartOne.pathsOfLength returns expected length-2 paths from bvb" {
                let actual = PartOne.pathsOfLength sampleGraph "bvb" 2

                let pathsThrough neighbour =
                    // Can ignore count because sample graph doesn't have repeated edges.
                    sampleGraph.[neighbour] |> Array.map (fun (n, _) -> [| "bvb", neighbour; neighbour, n |])

                let expected = [|
                    yield! pathsThrough "xhk"
                    yield! pathsThrough "hfx"
                    yield! pathsThrough "cmg"
                    yield! pathsThrough "rhn"
                    yield! pathsThrough "ntq"
                |]
                Assertions.test <@ actual |> Array.containsSameElements expected @>
            }

            test "PartOne.findNodesInSamePartition returns expected nodes for paths from bvb of length up to 2" {
                let actual = PartOne.findNodesInSamePartition 2 sampleGraph "bvb"

                // hfx paths - direct; via xhk; via rhn; via ntq.
                // xhk paths - direct; via hfx; via rhn; via ntq.
                Assertions.test <@ actual |> Array.containsSameElements [| "bvb"; "hfx"; "xhk" |] @>
            }

            test "PartOne.findNodesInSamePartition returns expected nodes for paths from bvb of length up to 3" {
                let actual = PartOne.findNodesInSamePartition 3 sampleGraph "bvb"

                // hfx paths - direct; via xhk; via rhn; via ntq.
                // xhk paths - direct; via hfx; via rhn; via ntq.
                // rhn paths - direct; via xhk; via hfx; via ntq, jqt.
                // ntq paths - direct; via xhk; via hfx; via rhn, jqt.
                // jqt paths - via xhk; via rhn; via ntq; via cmg, nvd, jqt.
                Assertions.test <@ actual |> Array.containsSameElements [| "bvb"; "hfx"; "xhk"; "rhn"; "ntq"; "jqt" |] @>
            }

            testList "PartOne.condense" [

                let inputResultsInExpectedOutput (nodes: string array) expected =
                    test $"given %A{nodes} returns expected graph" {
                        let actual =
                            System.String.Join(";", nodes)
                            |> PartOne.condense sampleGraph nodes
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

        testList "PartOne.pathsOfLength works with edge multiplicity" [

            let graph =
                [
                    "a", [| ("b", 1); ("c", 2); ("d", 1) |]
                    "b", [| ("a", 1); ("c", 1) |]
                    "c", [| ("a", 2); ("b", 1); ("d", 1) |]
                    "d", [| ("a", 1); ("c", 1) |]
                ]
                |> dict

            test "Length 1" {
                let actual = PartOne.pathsOfLength graph "a" 1

                let expected = [| [| "a", "b" |]; [| "a", "c" |]; [| "a", "c" |]; [| "a", "d" |] |]
                Assertions.test <@ actual |> Array.containsSameElements expected @>
            }

            test "Length 2" {
                let actual = PartOne.pathsOfLength graph "a" 2

                let expected = [|
                    [| ("a", "b"); ("b", "a") |]
                    [| ("a", "b"); ("b", "c") |]
                    [| ("a", "c"); ("c", "a") |]
                    [| ("a", "c"); ("c", "a") |]
                    [| ("a", "c"); ("c", "b") |]
                    [| ("a", "c"); ("c", "d") |]
                    [| ("a", "c"); ("c", "a") |]
                    [| ("a", "c"); ("c", "a") |]
                    [| ("a", "c"); ("c", "b") |]
                    [| ("a", "c"); ("c", "d") |]
                    [| ("a", "d"); ("d", "a") |]
                    [| ("a", "d"); ("d", "c") |]
                |]
                Assertions.test <@ actual |> Array.containsSameElements expected @>
            }

        ]

        testList "PartOne.shrink" [

            test "correct when only one shrink required" {
                let graph = [ "a", [| ("b", 4); ("c", 1) |]; "b", [| ("a", 4) |]; "c", [| ("a", 1) |] ] |> dict

                let actual =
                    PartOne.shrink graph "a"
                    |> Seq.toArray
                    |> Array.map (fun kvp -> kvp.Key, kvp.Value)

                Assertions.test <@ actual = [| "a;b", [| ("c", 1) |]; "c", [| ("a;b", 1) |] |] @>
            }

            test "correct when multiple shrinks required" {
                let graph =
                    [
                        "a", [| ("b", 4); ("c", 2); ("d", 1) |]
                        "b", [| ("a", 4); ("c", 2) |]
                        "c", [| ("a", 2); ("b", 2); ("d", 1) |]
                        "d", [| ("a", 1); ("c", 1) |]
                    ]
                    |> dict

                let actual =
                    PartOne.shrink graph "a"
                    |> Seq.toArray
                    |> Array.map (fun kvp -> kvp.Key, kvp.Value)

                Assertions.test <@ actual = [| "a;b;c", [| ("d", 2) |]; "d", [| ("a;b;c", 2) |] |] @>
            }

        ]

    ]
