module Day10

open System.Collections.Generic

type Direction =
    | North
    | South
    | East
    | West

module Direction =

    let opposite =
        function
        | North -> South
        | South -> North
        | East -> West
        | West -> East

type Tile =
    | Pipe of Direction * Direction
    | Ground
    | StartingPosition

let pTile =
    function
    | '|' -> Pipe (North, South)
    | '-' -> Pipe (East, West)
    | 'L' -> Pipe (North, East)
    | 'J' -> Pipe (North, West)
    | '7' -> Pipe (South, West)
    | 'F' -> Pipe (South, East)
    | '.' -> Ground
    | 'S' -> StartingPosition
    | c -> failwith $"Invalid tile char: '%c{c}"

[<RequireQualifiedAccess>]
module Tile =

    /// The directions that the pipe on the tile (if any) exits to.
    let exitDirections tile =
        match tile with
        | Pipe (a, b) -> [| a; b |]
        | Ground
        | StartingPosition -> [||]

    /// Whether the pipe on the tile (if any) has an exit in the given direction.
    let exits direction tile = tile |> exitDirections |> Array.contains direction

/// Parses lines into an array of pairs, whose first elements are coordinates and second elements are the contents
/// of the tile at those coordinates.
let parse lines =
    lines
    |> Array.mapi (fun i line -> line |> Seq.mapi (fun j c -> (i, j), pTile c) |> Seq.toArray)
    |> Array.collect id

/// Returns the coordinates of the starting tile, and a dictionary from coordinates to the tile contents. The value
/// corresponding to the starting coordinates is the pipe that is calculated to be present there, not
/// StartingPosition.
let calculateStartCoordsAndTileLookup tiles =
    let startCoords = tiles |> Array.find (snd >> (=) StartingPosition) |> fst
    let lookup = dict tiles

    let startPipe =
        let exitDirections = [
            let exits direction coords =
                coords |> lookup.TryGet |> Option.map (Tile.exits direction) |> Option.defaultValue false

            let i, j = startCoords

            if (i - 1, j) |> exits South then North
            if (i + 1, j) |> exits North then South
            if (i, j + 1) |> exits West then East
            if (i, j - 1) |> exits East then West
        ]

        Pipe (exitDirections.[0], exitDirections.[1])

    startCoords, dict (Array.append tiles [| startCoords, startPipe |])

/// Returns a sequence of pairs whose first element represents a distance from the starting position in the main
/// loop, and whose second element is the set of the coordinates which correspond to tiles at that distance.
let calculateCoordsByDistanceSeq startCoords (tileLookup: IDictionary<int * int, Tile>) =

    let startDirections = tileLookup.Item startCoords |> Tile.exitDirections

    let distanceGoingIn startDirection =

        ((startCoords, startDirection), Seq.initInfinite ignore)
        ||> Seq.scan (fun ((i, j), inDirection) _ ->
            let outDirection =
                tileLookup.Item (i, j)
                |> Tile.exitDirections
                |> Array.find ((<>) inDirection)

            let nextCoords =
                match outDirection with
                | North -> (i - 1, j)
                | South -> (i + 1, j)
                | East -> (i, j + 1)
                | West -> (i, j - 1)

            (nextCoords, outDirection |> Direction.opposite))
        |> Seq.map fst

    (distanceGoingIn startDirections.[0], distanceGoingIn startDirections.[1])
    ||> Seq.zip
    |> Seq.indexed
    // The paths are guaranteed to hit the midpoint of the loop at the same time because the length of the main loop
    // is even: the number of steps North equals the number of steps South, and likewise for East and West.
    |> Seq.takeUntilFirstMatch (fun (distance, (cs1, cs2)) -> distance <> 0 && cs1 = cs2)
    |> Seq.map (fun (distance, (cs1, cs2)) -> distance, Set (seq { cs1; cs2 }))

module PartOne =

    let solve lines =
        parse lines
        |> calculateStartCoordsAndTileLookup
        ||> calculateCoordsByDistanceSeq
        |> Seq.last
        |> fst

module PartTwo =

    // Consider the vertices of the square tiles that make up the grid.
    // The tiles which are not inside the loop are precisely those which have a vertex that can be reached by a path
    // starting from a vertex on the edge of the grid that traverses along sides that aren't crossed by the main
    // loop (which is to say that the main loop does not go directly between the two tiles that share the side).
    // To find all such vertices this function starts with the vertices at the edge of grid and iteratively adds
    // adjacent vertices that can be reached without crossing the main loop, until no new vertices are added.
    let solve lines =

        let startTileCoords, tileLookup = parse lines |> calculateStartCoordsAndTileLookup

        let rowCount = tileLookup.Keys |> Seq.map fst |> Seq.max
        let columnCount = tileLookup.Keys |> Seq.map snd |> Seq.max

        let mainLoopTileCoords =
            (startTileCoords, tileLookup)
            ||> calculateCoordsByDistanceSeq
            |> Seq.map snd
            |> Set.unionMany

        let accessibleAdjacentVertices (i, j) = seq {
            // Vertex (i, j) is a vertex of the tiles (i-1, j-1), (i-1, j), (i, j-1), (i, j) that are present in the
            // grid (edge vertices won't have four such tiles around them).
            // The adjacent vertices are (i-1, j), (i, j-1), (i, j+1) and (i+1, j).
            //                            Vertex (i-1, j)
            //                                 |
            //                                 v
            //                    x------------x------------x
            //                    | (i-1, j-1) | (i-1, j)   |
            // Vertex (i, j-1) -> x------------x------------x <- Vertex (i, j+1)
            //                    | (i, j-1)   | (i, j)     |
            //                    x------------x------------x
            //                                 ^
            //                                 |
            //                           Vertex (i+1, j)
            let tileI'J' = (i-1, j-1) |> fun cs -> cs, tileLookup.TryGet cs
            let tileI'J = (i-1, j) |> fun cs -> cs, tileLookup.TryGet cs
            let tileIJ' = (i, j-1) |> fun cs -> cs, tileLookup.TryGet cs
            let tileIJ = (i, j) |> fun cs -> cs, tileLookup.TryGet cs

            let connectedByMainLoop (((cs1, t1), d1), ((cs2, t2), d2)) =
                mainLoopTileCoords |> Set.contains cs1
                && mainLoopTileCoords |> Set.contains cs2
                &&
                    (t1, t2)
                    ||> Option.map2 (fun t1 t2 -> t1 |> Tile.exits d1 && t2 |> Tile.exits d2)
                    |> Option.defaultValue false

            // Vertex (i-1, j) can be reached if there is no East-West pipe between tiles (i-1, j-1) and (i-1, j).
            if i > 0 && not (connectedByMainLoop ((tileI'J', East), (tileI'J, West))) then (i-1, j)

            // Vertex (i, j-1) can be reached if there is no North-South pipe between tiles (i-1, j-1) and (i, j-1).
            if j > 0 && not (connectedByMainLoop ((tileI'J', South), (tileIJ', North))) then (i, j-1)

            // Vertex (i, j+1) can be reached if there is no North-South pipe between tiles (i-1, j) and (i, j).
            if j < columnCount && not (connectedByMainLoop ((tileI'J, South), (tileIJ, North))) then (i, j+1)

            // Vertex (i+1, j) can be reached if there is no East-West pipe between tiles (i, j-1) and (i, j).
            if i < rowCount && not (connectedByMainLoop ((tileIJ', East), (tileIJ, West))) then (i+1, j)
        }

        let edgeVertices =
            Seq.allPairs (seq { 0 .. rowCount + 1 }) (seq { 0 .. columnCount + 1 }) |> Set
            |> Set.filter (fun (i, j) -> i = 0 || i = rowCount + 1 || j = 0 || j = columnCount + 1)

        let unenclosedVertices =
            // Start with the vertices at the edge of grid.
            ((Set.empty, edgeVertices), Seq.initInfinite ignore)
            // Iteratively add all adjacent vertices that can be reached without crossing the main loop
            ||> Seq.scan
                (fun (oldVertices, newVertices) _ ->
                    let verticesSoFar = Set.union oldVertices newVertices
                    let nextVertices =
                        newVertices
                        |> Seq.collect accessibleAdjacentVertices
                        |> Set
                        |> fun s -> Set.difference s verticesSoFar
                    verticesSoFar, nextVertices)
            // Until no new vertices are added.
            |> Seq.takeWhile (snd >> Set.isEmpty >> not)
            |> Seq.last
            ||> Set.union

        let enclosedTileCoords =
            let allTileCoords = Seq.allPairs (seq { 0 .. rowCount }) (seq { 0 .. columnCount }) |> Set

            unenclosedVertices
            |> Seq.map (fun (i, j) -> Set (seq { (i-1, j); (i-1, j-1); (i, j-1); (i, j) }))
            |> Set.unionMany
            |> fun unenclosedTileCoords -> Set.difference allTileCoords unenclosedTileCoords

        enclosedTileCoords |> Set.count

