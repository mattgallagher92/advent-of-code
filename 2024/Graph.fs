module Graph

open System.Collections.Generic

// TODO: add tests
// Test input:
// (29, 13)
// (47, 13)
// (47, 29)
// (47, 53)
// (47, 61)
// (53, 13)
// (53, 29)
// (61, 13)
// (61, 29)
// (61, 53)
// (75, 13)
// (75, 29)
// (75, 47)
// (75, 53)
// (75, 61)
// (97, 13)
// (97, 29)
// (97, 47)
// (97, 53)
// (97, 61)
// (97, 75)
// Expected output:
// Graph keys: [|13; 29; 47; 53; 61; 75; 97|]
// Adjacent to 13: [||]
// Adjacent to 29: [|13|]
// Adjacent to 47: [|13; 29; 53; 61|]
// Adjacent to 53: [|13; 29|]
// Adjacent to 61: [|13; 29; 53|]
// Adjacent to 75: [|13; 29; 47; 53; 61|]
// Adjacent to 97: [|13; 29; 47; 53; 61; 75|]

/// Converts a graph represented as a list of edges (themselves represented as pairs where the first element is the
/// start node and the second element is the end node) into the same graph represented as a dictionary from each node
/// to an array of the nodes that can be reached from it along a single edge.
let edgeArrayToAdjacencyArrays (edges: ('a * 'a) seq) =
    let edges = edges |> Seq.toArray

    let allNodes =
        (edges |> Array.map fst, edges |> Array.map snd)
        ||> Array.append
        |> Array.distinct

    let nonEmptyAdjacenyArrays =
        edges |> Array.groupBy fst |> Array.map (fun (n, es) -> n, es |> Array.map snd)

    let emptyAdjacenyArrays =
        allNodes
        |> Array.except (nonEmptyAdjacenyArrays |> Array.map fst)
        |> Array.map (fun n -> n, [||])

    (nonEmptyAdjacenyArrays, emptyAdjacenyArrays) ||> Array.append |> dict

// TODO: add tests.
/// Returns the subgraph of graph with nodes restricted to those given as an argument
let subgraph (nodes: 'a seq) (graph: IDictionary<'a, 'a array>) =
    let nodesSet = HashSet nodes

    graph
    |> Seq.filter (_.Key >> nodesSet.Contains)
    |> Seq.map (fun kvp -> kvp.Key, kvp.Value |> Array.filter nodesSet.Contains)
    |> dict

[<RequireQualifiedAccess>]
type private Mark =
    | Temp
    | Perm

// TODO: add tests.
// Test input: output from above test.
// Expected output: [ 97; 75; 47; 61; 53; 29; 13 ]

/// Returns a list containing all nodes in the given direct acyclic graph where, for every edge in the graph, the node
/// at the head of the edge comes earlier in the list than the node at the tail of that edge.
/// Throws if the graph has cycles.
// Uses the algorithm described at https://en.wikipedia.org/wiki/Topological_sorting#Depth-first_search.
let topologicalSort (dag: IDictionary<'a, 'a array>) =
    let mutable l = []

    let unmarked = HashSet dag.Keys
    let marks = Dictionary<'a, Mark>()

    let rec visit node =
        match marks.TryGet node with
        | Some Mark.Temp -> failwith $"Graph has cycles! %A{dag}"
        | Some Mark.Perm -> ()
        | None ->
            unmarked.Remove node |> ignore
            marks[node] <- Mark.Temp

            for neighbour in dag[node] do
                visit neighbour

            marks[node] <- Mark.Perm
            l <- node :: l

    while unmarked.Count > 0 do
        let n = unmarked |> Seq.head
        visit n

    l
