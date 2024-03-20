module Day18

open System

type Direction =
    | Up
    | Down
    | Left
    | Right

type PlanStep = {
    Direction: Direction
    Metres: int
}

module PartOne =

    let calculateEdgeCoords plan =
        ([| (0, 0) |], plan)
        ||> Array.fold (fun edgeCoords step ->

            let row, col = edgeCoords |> Array.last

            let newCoords =
                match step.Direction with
                | Up -> [| 1 .. step.Metres |] |> Array.map (fun i -> (row - i, col))
                | Down -> [| 1 .. step.Metres |] |> Array.map (fun i -> (row + i, col))
                | Left -> [| 1 .. step.Metres |] |> Array.map (fun i -> (row, col - i))
                | Right -> [| 1 .. step.Metres |] |> Array.map (fun i -> (row, col + i))

            Array.append edgeCoords newCoords)

    // Uses breadth-first search to determine squares outside the trench.
    let calculateExteriorSquares
        trenchEdgeCoords
        (boundingRectangleTop, boundingRectangleBottom, boundingRectangleLeft, boundingRectangleRight)
        =

        let exteriorSquaresOnRectangleEdge =
            [|

                for col in boundingRectangleLeft .. boundingRectangleRight do
                    boundingRectangleTop, col
                    boundingRectangleBottom, col

                for row in boundingRectangleTop .. boundingRectangleBottom do
                    row, boundingRectangleLeft
                    row, boundingRectangleRight

            |]
            |> Array.except trenchEdgeCoords

        let adjacentSquares (row, col) = [|
            if row > boundingRectangleTop then row - 1, col
            if row < boundingRectangleBottom then row + 1, col
            if col > boundingRectangleLeft then row, col - 1
            if col < boundingRectangleRight then row, col + 1
        |]

        ([||], exteriorSquaresOnRectangleEdge)
        |> Array.unfold (fun (earlierExteriorSquares, previousExteriorSquares) ->

            let exteriorSquaresSoFar = Array.append previousExteriorSquares earlierExteriorSquares

            let nextExteriorSquares =
                previousExteriorSquares
                |> Array.collect adjacentSquares
                |> Array.except exteriorSquaresSoFar
                |> Array.except trenchEdgeCoords

            match nextExteriorSquares with
            | [||] -> None
            | ss -> Some (Array.append ss exteriorSquaresSoFar, (exteriorSquaresSoFar, ss)))

        |> Array.last

    let solveFromPlan plan =

        let trenchEdgeCoords = plan |> calculateEdgeCoords

        let boundingRectangleTop, boundingRectangleBottom, boundingRectangleLeft, boundingRectangleRight =
            let rows = trenchEdgeCoords |> Seq.map fst
            let cols = trenchEdgeCoords |> Seq.map snd
            rows |> Seq.min, rows |> Seq.max, cols |> Seq.min, cols |> Seq.max

        let exteriorSquares =
            calculateExteriorSquares
                trenchEdgeCoords
                (boundingRectangleTop, boundingRectangleBottom, boundingRectangleLeft, boundingRectangleRight)

        // Return bounding rectangle area less number of exterior squares.
        ((boundingRectangleRight - boundingRectangleLeft + 1) |> int64)
        * ((boundingRectangleBottom - boundingRectangleTop + 1) |> int64)
        - exteriorSquares.LongLength

    let parseLines (lines: string array) =
        lines
        |> Array.map (fun line ->
            let parts = line.Split(' ')
            {
                Direction =
                    match parts.[0].[0] with
                    | 'U' -> Up
                    | 'D' -> Down
                    | 'L' -> Left
                    | 'R' -> Right
                    | c -> failwith $"Invalid direction char %c{c}"
                Metres = parts.[1] |> Int32.Parse
            })

    let solve lines = lines |> parseLines |> solveFromPlan

module PartTwo =

    let parseLines (lines: string array) =
        lines
        |> Array.map (fun line ->
            let parts = line.Split(' ')
            let hexCode = parts.[2].Substring(2, 5)
            let dirCode = parts.[2].[7]
            {
                Direction =
                    match dirCode with
                    | '0' -> Right
                    | '1' -> Down
                    | '2' -> Left
                    | '3' -> Up
                    | c -> failwith $"Invalid direction code %c{c}"
                Metres = Convert.ToInt32(hexCode, 16)
            })

    type HorizontalEdge = {
        Row: int
        Left: int
        Right: int
    }

    type VerticalEdge = {
        Col: int
        Top: int
        Bottom: int
    }

    let calculateEdges steps =

        let _, horizontalEdges, verticalEdges =
            (((0, 0), [], []), steps)
            ||> Array.fold (fun ((startRow, startCol), hs, vs) ->
                function
                | { Direction = Up; Metres = m } ->
                    { Col = startCol; Top = startRow - m; Bottom = startRow }
                    |> fun newV -> (startRow - m, startCol), hs, newV :: vs
                | { Direction = Down; Metres = m } ->
                    { Col = startCol; Top = startRow; Bottom = startRow + m }
                    |> fun newV -> (startRow + m, startCol), hs, newV :: vs
                | { Direction = Left; Metres = m } ->
                    { Row = startRow; Left = startCol - m; Right = startCol }
                    |> fun newH -> (startRow, startCol - m), newH :: hs, vs
                | { Direction = Right; Metres = m } ->
                    { Row = startRow; Left = startCol; Right = startCol + m }
                    |> fun newH -> (startRow, startCol + m), newH :: hs, vs)

        horizontalEdges, verticalEdges

    let internalSquaresInRow verticalEdges row =

        let rec inner interiorStart squaresSoFar vEdges =

            match interiorStart, vEdges with
            // Outside shape and no more edges.
            | None, [] ->
                squaresSoFar

            // Outside shape before vl; horizontal edge between vl and vr; inside shape after vr.
            | None, vl :: vr :: vs when vl.Bottom = row && vr.Top = row || vl.Top = row && vr.Bottom = row ->
                inner (Some vl.Col) squaresSoFar vs

            // Outside shape before vl; horizontal edge between vl and vr; outside shape after vr.
            | None, vl :: vr :: vs when vl.Bottom = row && vr.Bottom = row || vl.Top = row && vr.Top = row ->
                inner None (squaresSoFar + vr.Col - vl.Col + 1) vs

            // Outside shape before v; inside after v.
            | None, v :: vs ->
                inner (Some v.Col) squaresSoFar vs

            // Inside shape and no more edges; invalid.
            | Some c, [] ->
                failwith $"Interior opened at (%i{row}, %i{c}) and never closed."

            // Inside shape before vl; horizontal edge between vl and vr; outside shape after vr.
            | Some c, vl :: vr :: vs when vl.Bottom = row && vr.Top = row || vl.Top = row && vr.Bottom = row ->
                inner None (squaresSoFar + vr.Col - c + 1) vs

            // Inside shape before vl; horizontal edge between vl and vr; inside shape after vr.
            | Some c, vl :: vr :: vs when vl.Bottom = row && vr.Bottom = row || vl.Top = row && vr.Top = row ->
                inner (Some c) squaresSoFar vs

            // Inside shape before v; outside after v.
            | Some c, v :: vs ->
                inner None (squaresSoFar + v.Col - c + 1) vs

        verticalEdges
        |> List.filter (fun v -> v.Top <= row && v.Bottom >= row)
        |> List.sortBy _.Col
        |> inner None 0
        |> int64

    /// Calculates the total area of the bands between the rows with horizontal edges.
    let areaInRowsBetweenHorizontalEdges rowsWithHorizontalEdges verticalEdgesLeftToRight=
        rowsWithHorizontalEdges
        |> Seq.sort
        // Split the shape into bands between horizontal edges (not including those edges).
        |> Seq.pairwise
        // Map to the total area of the parts of the shape inside the band.
        |> Seq.map (fun (bandTopRow, bandBottomRow) ->
            verticalEdgesLeftToRight
            // Only consider the vertical edges that split the band.
            |> List.filter (fun v -> v.Top <= bandTopRow && v.Bottom >= bandBottomRow)
            // Consider the rectangles bounded by such edges.
            |> List.pairwise
            // The rectangles alternate between inside and outside the shape.
            |> List.mapi (fun i ({ Col = l }, { Col = r }) -> (if i % 2 = 0 then r - l + 1 else 0) |> int64)
            // This sum is the total width of the band's rectangles inside the shape.
            |> List.sum
            // Multiplying by the band height gives the total area of the parts of the shape inside the band.
            |> (*) ((bandBottomRow - bandTopRow - 1) |> int64))
        |> Seq.sum

    let solve lines =

        let rowsWithHorizontalEdges, verticalEdgesLeftToRight =
            let horizontalEdges, verticalEdges = lines |> parseLines |> calculateEdges
            horizontalEdges |> Seq.map _.Row |> Seq.distinct |> Seq.toArray, verticalEdges |> List.sortBy _.Col

        let areaInRowsWithHorizontalEdges =
            rowsWithHorizontalEdges |> Array.sumBy (internalSquaresInRow verticalEdgesLeftToRight)

        let areaInRowsBetweenHorizontalEdges =
            areaInRowsBetweenHorizontalEdges rowsWithHorizontalEdges verticalEdgesLeftToRight

        areaInRowsWithHorizontalEdges + areaInRowsBetweenHorizontalEdges

