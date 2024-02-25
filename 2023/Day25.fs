module Day25

module PartOne =

    let solve lines =
        // 0. Make a graph of the connected components (nodes) and wires (edges).
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

            test "PartOne.solve gives correct answer" {
                let actual = PartOne.solve sampleInput

                Assertions.test <@ actual = 54 @>
            }

        ]

    ]
