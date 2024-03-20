module Day17

open System
open System.Collections.Generic

type Direction =
    | Up
    | Down
    | Left
    | Right

module Direction =

    let orthogonal =
        function
        | Up | Down -> [| Left; Right |]
        | Left | Right -> [| Up; Down |]

type Node = {
    Location: int * int
    // Optional because there is no approach for the start node.
    /// int represents number of steps in that direction leading up to the location.
    Approach: (Direction * int) option
}

let locationInDirection numRows numCols (row, col) direction =
    match direction, row, col with
    | Up, 0, _ -> None
    | Up, row, col -> Some (row - 1, col)
    | Down, row, _ when row = numRows - 1 -> None
    | Down, row, col -> Some (row + 1, col)
    | Left, _, 0 -> None
    | Left, row, col -> Some (row, col - 1)
    | Right, _, col when col = numCols - 1 -> None
    | Right, row, col -> Some (row, col + 1)

let nextNodes numRows numCols canContinueInSameDirection canMakeQuarterTurn node =

    match node.Approach, node.Location with
    | None, (0, 0) ->
        seq {
            { Location = (0, 1); Approach = Some (Right, 1) }
            { Location = (1, 0); Approach = Some (Down, 1) }
        }

    | None, loc ->
        failwith $"Non-start node at location %A{loc} has no approach"

    | Some (entryDir, stepCount), loc ->
        seq {

            if canMakeQuarterTurn stepCount then
                yield!
                    entryDir
                    |> Direction.orthogonal
                    |> Seq.map (fun exitDir ->
                        locationInDirection numRows numCols loc exitDir
                        |> Option.map (fun newLoc -> { Location = newLoc; Approach = Some (exitDir, 1) }))

            if canContinueInSameDirection stepCount then
                locationInDirection numRows numCols loc entryDir
                |> Option.map (fun newLoc ->
                    { Location = newLoc; Approach = Some (entryDir, stepCount + 1) })

        }
        |> Seq.choose id

// Uses Dijkstra's algorithm.
let solve nextNodes isEndNode (map: int array2d) =

    let startNode = { Location = (0, 0); Approach = None }

    let nodesByHeatLoss = PriorityQueue<Node, int>()
    nodesByHeatLoss.Enqueue(startNode, 0)
    let minHeatLossTo = Dictionary<Node, int> [ KeyValuePair (startNode, 0) ]
    let mutable minHeatLossToEndLocation : int option = None

    while minHeatLossToEndLocation.IsNone && nodesByHeatLoss.Count > 0 do

        match nodesByHeatLoss.Dequeue() with
        | current when isEndNode current ->
            minHeatLossToEndLocation <- Some minHeatLossTo.[current]

        | current ->

            current
            |> nextNodes
            |> Seq.iter (fun ({ Location = row, col } as next) ->

                let newHeatLoss = minHeatLossTo.[current] + map.[ row, col ]

                let nodeAlreadyVisitedWithLessHeatLoss =
                    next
                    |> minHeatLossTo.TryGet
                    |> Option.map (fun minHeatLoss -> minHeatLoss <= newHeatLoss)
                    |> Option.defaultValue false

                if nodeAlreadyVisitedWithLessHeatLoss then
                    ()
                else
                    minHeatLossTo.[next] <- newHeatLoss
                    nodesByHeatLoss.Enqueue(next, newHeatLoss))

    minHeatLossToEndLocation

module PartOne =

    let solve lines =
        lines
        |> array2D
        |> Array2D.map (Char.ToString >> Int32.Parse)
        |> fun map ->
            let numRows, numCols = map |> Array2D.length1, map |> Array2D.length2
            let nextNodes = nextNodes numRows numCols (fun stepCount -> stepCount < 3) (fun _ -> true)
            let isEndNode =
                fun { Location = row, col } ->
                    row = numRows - 1 && col = numCols - 1

            solve nextNodes isEndNode map

module PartTwo =

    let solve lines =
        lines
        |> array2D
        |> Array2D.map (Char.ToString >> Int32.Parse)
        |> fun map ->

            let numRows, numCols = map |> Array2D.length1, map |> Array2D.length2

            let nextNodes =
                nextNodes numRows numCols (fun stepCount -> stepCount < 10) (fun stepCount -> stepCount > 3)

            let isEndNode ({ Location = row, col } as node) =
                let isEndLocation = row = numRows - 1 && col = numCols - 1
                let canStop =
                    node.Approach
                    |> Option.map (fun (_, stepCount) -> stepCount >= 4)
                    |> Option.defaultValue false

                isEndLocation && canStop

            solve nextNodes isEndNode map

