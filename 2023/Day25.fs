module Day25

let parse lines =
    lines
    |> Array.collect (fun (line: string) ->
        let parts = line.Split(": ")
        let a = parts.[0]
        let bs = parts.[1].Split(" ")
        [| for b in bs do a, b; b, a |])
    |> Array.groupBy fst
    |> Array.map (fun (node, edges) -> node, edges |> Array.map snd)
    |> dict

module PartOne =

    let solve lines =
        lines
        // 0. Make a graph of the connected components (nodes) and wires (edges).
        |> parse
        // 1. Choose a start node. Alphabetical order might be useful for determinism.
        // 2. Calculate paths of a predetermined length. Uneducated guess: 7 steps will finish relatively quickly.
        // 3. Remove duplicate edges from the end so that they are paths without repeated edges.
        // 4. Find the destination nodes that have more than 3 independent paths (with no common edges) from the start
        // node to them. Those nodes must be in the same partition (connected group).
        // 5. Make a new (multi-) graph where the partition is reduced to a single node. Ensure that any nodes were
        // connected to multiple nodes in the partition now have multiple edges connecting it to the partition; if there
        // are more than 3 they can be folded into the partition.
        // 6. Repeat steps 1-5, with a node outside of the new partition, until some stop condition. Perhaps 10
        // consecutive iterations without reducing the number of partitions or reduced to two partitions.
        // 7. If more than 2 partitions, brute force check. One possible approach is to split the nodes into two groups
        // and check whether there are exactly 3 inter-partition edges. There are 2^n possible partitions, so might want
        // to throw if n is too large.
        0

module Test =

    open Swensen.Unquote
    open Expecto

    [<AutoOpen>]
    module Util =

        let containsSameElements<'a when 'a : comparison> (xs: 'a seq) (ys: 'a seq) =
            let equalLength () =
                if Seq.length xs = Seq.length ys then
                    true
                else
                    failwith $"%A{xs} is different length to %A{ys}"
            let sameElements () =
                (Seq.sort xs, Seq.sort ys)
                ||> Seq.forall2 (fun x y -> if x = y then true else failwith $"%A{x} <> %A{y}")
            equalLength () && sameElements ()

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

            let sampleMap =
                [|
                    "jqt", [| "rhn"; "xhk"; "nvd"; "ntq" |]
                    "rsh", [| "frs"; "pzl"; "lsr"; "rzs" |]
                    "xhk", [| "hfx"; "jqt"; "rhn"; "bvb"; "ntq" |]
                    "cmg", [| "qnr"; "nvd"; "lhk"; "bvb"; "rzs" |]
                    "rhn", [| "xhk"; "bvb"; "hfx"; "jqt" |]
                    "bvb", [| "xhk"; "hfx"; "cmg"; "rhn"; "ntq" |]
                    "pzl", [| "lsr"; "hfx"; "nvd"; "rsh" |]
                    "qnr", [| "nvd"; "cmg"; "rzs"; "frs" |]
                    "ntq", [| "jqt"; "hfx"; "bvb"; "xhk" |]
                    "nvd", [| "lhk"; "jqt"; "cmg"; "pzl"; "qnr" |]
                    "lsr", [| "lhk"; "rsh"; "pzl"; "rzs"; "frs" |]
                    "rzs", [| "qnr"; "cmg"; "lsr"; "rsh" |]
                    "frs", [| "qnr"; "lhk"; "lsr"; "rsh" |]
                    "hfx", [| "xhk"; "rhn"; "bvb"; "pzl"; "ntq" |]
                    "lhk", [| "cmg"; "nvd"; "lsr"; "frs" |]
                |]
                |> dict

            test "Parse returns expected Map" {
                let actual = parse sampleInput

                let actualKeys = actual.Keys |> Seq.toArray
                Assertions.test
                    <@
                        containsSameElements actualKeys nodes
                        && actualKeys |> Array.forall (fun key -> containsSameElements actual.[key] sampleMap.[key])
                    @>
            }

            test "PartOne.solve gives correct answer" {
                let actual = PartOne.solve sampleInput

                Assertions.test <@ actual = 54 @>
            }

        ]

    ]
