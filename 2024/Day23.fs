module Day23

let edges (lines: string array) =
    lines
    |> Array.collect (fun s -> [| s[0..1], s[3..4]; s[3..4], s[0..1] |])
    |> set

let vertices edges =
    edges |> Set.map (fun (a, b) -> set [ a; b ]) |> Set.unionMany

let graph edges =
    edges
    |> Seq.toArray
    |> Array.groupBy fst
    |> Array.mapSnd (Array.map snd >> set)
    |> dict

module PartOne =

    let findConnectedTriples predicate lines =
        let es = edges lines
        let vs = vertices es
        let g = graph es

        let isEdge e = es |> Set.contains e

        vs
        |> Set.filter predicate
        |> Seq.collect (fun v ->
            let neighbours = g[v]

            neighbours
            |> Seq.allPairs neighbours
            |> Seq.choose (fun (n1, n2) -> if isEdge (n1, n2) then Some(v, n1, n2) else None))
        // Deduplicate triples by sorting names.
        |> Seq.map (fun (a, b, c) ->
            let sorted = [| a; b; c |] |> Array.sort
            sorted[0], sorted[1], sorted[2])
        |> set

    let solve (lines: string array) =
        lines |> findConnectedTriples (fun s -> s.StartsWith 't') |> _.Count

module PartTwo =

    let rec maxCliqueContaining subClique edges leftToTest =
        match leftToTest with
        | [] -> subClique
        | h :: t when subClique |> List.forall (fun v -> edges |> Set.contains (v, h)) ->
            (edges, t) ||> maxCliqueContaining (h :: subClique)
        | _ :: t -> (edges, t) ||> maxCliqueContaining subClique

    let maxClique edges =
        let vertices = vertices edges

        vertices
        |> Seq.map (fun v -> (edges, Set.toList vertices) ||> maxCliqueContaining [ v ])
        |> Seq.maxBy _.Length

    let password maxClique =
        maxClique |> List.sort |> String.concat ","

    let solve (lines: string array) =
        let lanParty = lines |> edges |> maxClique
        printfn $"The password is '%s{password lanParty}'"
        lanParty.Length

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

            testCase "correct triples are found from sample input" (fun _ ->
                test
                    <@
                        sampleInput |> PartOne.findConnectedTriples (fun s -> s.StartsWith 't') = set [
                            "co", "de", "ta"
                            "co", "ka", "ta"
                            "de", "ka", "ta"
                            "qp", "td", "wh"
                            "tb", "vc", "wq"
                            "tc", "td", "wh"
                            "td", "wh", "yn"
                        ]
                    @>)

            testList "PartOne" [
                testCase "solve works with sample input" (fun _ -> test <@ PartOne.solve sampleInput = 7 @>)
            ]

            testList "PartTwo" [
                testCase "works with sample input" (fun _ ->
                    test <@ sampleInput |> edges |> PartTwo.maxClique |> PartTwo.password = "co,de,ka,ta" @>)
            ]
        ]

let dayFns = {
    Tests = Test.all
    UtilTests = []
    PartOne = PartOne.solve >> int64
    PartTwo = PartTwo.solve >> int64
}
