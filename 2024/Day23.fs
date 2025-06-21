module Day23

open System.Collections.Generic

module PartOne =

    /// Keys are computer names, values are sets of names of computers that key computer is connected to.
    // TODO: could we save some computation time by avoiding storing duplicate connections? E.g. use alphabetical order.
    let parseConnectionGraph (lines: string array) =
        lines
        |> Array.map (fun s -> s[0..1], s[3..4])
        |> Array.collect (fun (a, b) -> [| a, b; b, a |])
        |> Array.groupBy fst
        |> Array.mapSnd (Array.map snd >> set)
        |> dict

    /// Returned computer names are in alphabetical order, for uniqueness.
    // TODO: terser name
    let findConnectedTriplesContainingComputerWhoseNameStartWithT (conns: IDictionary<string, Set<string>>) =
        conns
        |> Seq.collect (fun kvp ->
            // For every key starting with t,
            if kvp.Key.StartsWith 't' then
                // For all pairs from connected set,
                kvp.Value
                |> Seq.allPairs kvp.Value
                |> Seq.collect (fun (c1, c2) ->
                    // Check whether the second is in the first's connected set. If so, that's a connected triple.
                    if conns[c1] |> Set.contains c2 then
                        [| (kvp.Key, c1, c2) |]
                    else
                        [||])
            else
                [||])
        // Deduplicate triples by sorting names.
        |> Seq.map (fun (a, b, c) ->
            let sorted = [| a; b; c |] |> Array.sort
            sorted[0], sorted[1], sorted[2])
        |> set

    let solve (lines: string array) =
        lines
        |> parseConnectionGraph
        |> findConnectedTriplesContainingComputerWhoseNameStartWithT
        |> Seq.length

/// See https://en.wikipedia.org/wiki/Disjoint-set_data_structure.
// TODO: move
type DisjointSet<'a when 'a: equality>() =
    let parent = Dictionary<'a, 'a>()
    let size = Dictionary<'a, int>()

    member private this.Find x =
        if not (parent.ContainsKey x) then
            parent[x] <- x
            size[x] <- 1

        if parent[x] <> x then
            // Compress paths to make navigation to root quicker in future.
            parent[x] <- this.Find(parent[x])

        parent[x]

    /// Indicates that two nodes are part of the same component, creating nodes if necessary.
    member this.Union x y =
        let xRoot = this.Find x
        let yRoot = this.Find y

        if xRoot <> yRoot then
            if size[xRoot] > size[yRoot] then
                parent[yRoot] <- xRoot
                size[xRoot] <- size[yRoot] + size[xRoot]
            else
                parent[xRoot] <- yRoot
                size[yRoot] <- size[xRoot] + size[yRoot]

    /// Whether two nodes are part of the same component.
    member this.IsConnectedTo x y = this.Find x = this.Find y

    member _.LargestConnectedComponent() =
        let root = size |> Seq.maxBy _.Value |> _.Key

        parent
        |> Seq.choose (fun kvp -> if kvp.Value = root then Some kvp.Key else None)

module PartTwo =

    let password (lines: string array) =

        // TODO: Whoops! This gets the maximum connected component, but we want the maximum complete subgraph (aka maximum clique).
        let cs =
            let ds = DisjointSet<_>()
            lines |> Array.iter (fun s -> ds.Union (s[0..1]) (s[3..4]))
            ds.LargestConnectedComponent() |> Seq.sort


        System.String.Join(',', cs)

    let solve (lines: string array) =

        lines |> password |> printfn "The password is '%s'"
        -1

module Test =

    open Expecto
    open Swensen.Unquote

    let all =
        testList "Day23" [
            let sampleInput = [|
                "kh-tc"
                "qp-kh"
                "de-cg"
                "ka-co"
                "yn-aq"
                "qp-ub"
                "cg-tb"
                "vc-aq"
                "tb-ka"
                "wh-tc"
                "yn-cg"
                "kh-ub"
                "ta-co"
                "de-co"
                "tc-td"
                "tb-wq"
                "wh-td"
                "ta-ka"
                "td-qp"
                "aq-cg"
                "wq-ub"
                "ub-vc"
                "de-ta"
                "wq-aq"
                "wq-vc"
                "wh-yn"
                "ka-de"
                "kh-ta"
                "co-tc"
                "wh-qp"
                "tb-vc"
                "td-yn"
            |]

            testList "PartOne" [
                testCase "correct triples are found from sample input" (fun _ ->
                    test
                        <@
                            sampleInput
                            |> PartOne.parseConnectionGraph
                            |> PartOne.findConnectedTriplesContainingComputerWhoseNameStartWithT = set [
                                "co", "de", "ta"
                                "co", "ka", "ta"
                                "de", "ka", "ta"
                                "qp", "td", "wh"
                                "tb", "vc", "wq"
                                "tc", "td", "wh"
                                "td", "wh", "yn"
                            ]
                        @>)

                testCase "solve works with sample input" (fun _ -> test <@ PartOne.solve sampleInput = 7 @>)
            ]

            testList "PartTwo" [
                testCase "password works with sample input" (fun _ ->
                    test <@ PartTwo.password sampleInput = "co,de,ka,ta" @>)
            ]
        ]

let dayFns = {
    Tests = Test.all
    UtilTests = []
    PartOne = PartOne.solve >> int64
    PartTwo = PartTwo.solve >> int64
}
