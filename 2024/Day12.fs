module Day12

let parse (lines: string array) =
    lines |> Array.map Seq.toArray |> array2D

type Region = { PlantType: char; PlotCoordinates: (int * int) Set }

let area region = region.PlotCoordinates.Count

let findRegionFor map startCoords =
    let get pos = pos ||> Array2D.get map
    let adjs pos = pos |> Array2D.adjacentIndexes map
    let plantType = get startCoords

    (set [| startCoords |], set [| startCoords |])
    |> Array.unfold (fun (coordinatesSoFar, lastCoordinatesAdded) ->
        match
            lastCoordinatesAdded
            |> Set.map (adjs >> Array.filter (get >> (=) plantType) >> set)
            |> Set.unionMany
            |> fun new' -> Set.difference new' coordinatesSoFar
        with
        | s when Set.isEmpty s -> None
        | newCoords ->
            let nextCoordinatesSoFar = Set.union coordinatesSoFar newCoords
            Some(nextCoordinatesSoFar, (nextCoordinatesSoFar, newCoords)))
    |> function
        | [||] -> set [| startCoords |]
        | coordsArray -> Array.last coordsArray
    |> fun coords -> { PlantType = plantType; PlotCoordinates = coords }

let calculateRegions map =
    let coords = map |> Array2D.indexes

    (set coords, set [||])
    |> Array.unfold (fun (uncoveredCoords, regionsSoFar) ->
        Seq.tryHead uncoveredCoords
        |> Option.map (fun u ->
            let newRegion = findRegionFor map u
            let newRegions = regionsSoFar |> Set.add newRegion
            let newUncovered = Set.difference uncoveredCoords newRegion.PlotCoordinates
            newRegions, (newUncovered, newRegions)))
    |> Array.last

type Direction =
    | Up
    | Down
    | Left
    | Right

    member this.Next =
        match this with
        | Right -> Up
        | Up -> Left
        | Left -> Down
        | Down -> Right

let nextInDirection direction (r, c) =
    match direction with
    | Up -> r - 1, c
    | Down -> r + 1, c
    | Left -> r, c - 1
    | Right -> r, c + 1

let perimterParts { PlotCoordinates = coords } =
    coords
    |> Seq.toArray
    |> Array.collect (fun cs -> [|
        for d in [| Up; Down; Left; Right |] do
            if cs |> nextInDirection d |> coords.Contains |> not then
                cs, d
    |])

module PartOne =

    let perimeter = perimterParts >> Array.length

    let fencingPrice region = area region * perimeter region

    let solve lines =
        lines |> parse |> calculateRegions |> Seq.sumBy fencingPrice

module PartTwo =

    let numberOfSides region =
        let parts = region |> perimterParts

        parts
        // Remove all but one part from each side. For example, for an "Up" side, remove all but the rightmost plot.
        |> Array.except (parts |> Array.map (fun (cs, d) -> cs |> nextInDirection d.Next, d))
        |> Array.length

    let bulkDiscountPrice region = area region * numberOfSides region

    let solve (lines: string array) =
        lines |> parse |> calculateRegions |> Seq.sumBy bulkDiscountPrice

module Test =

    open Expecto
    open Swensen.Unquote

    let all =
        testList "Day12" [
            let sample1 = [| "AAAA"; "BBCD"; "BBCC"; "EEEC" |]
            let sample2 = [| "OOOOO"; "OXOXO"; "OOOOO"; "OXOXO"; "OOOOO" |]

            let sample3 = [|
                "RRRRIICCFF"
                "RRRRIICCCF"
                "VVRRRCCFFF"
                "VVRCCCJFFF"
                "VVVVCJJCFE"
                "VVIVCCJJEE"
                "VVIIICJJEE"
                "MIIIIIJJEE"
                "MIIISIJEEE"
                "MMMISSJEEE"
            |]

            let sample4 = [| "EEEEE"; "EXXXX"; "EEEEE"; "EXXXX"; "EEEEE" |]
            let sample5 = [| "AAAAAA"; "AAABBA"; "AAABBA"; "ABBAAA"; "ABBAAA"; "AAAAAA" |]

            let map1 = parse sample1
            let map2 = parse sample2
            let map3 = parse sample3
            let map4 = parse sample4
            let map5 = parse sample5

            testList "PartOne" [
                let testAreasAndPerimeters mapName map expected =
                    testCase $"%s{mapName} regions have correct areas and perimeters" (fun _ ->
                        let result = map |> calculateRegions

                        let toCompare =
                            result
                            |> Set.map (fun region -> region.PlantType, area region, PartOne.perimeter region)

                        test <@ toCompare = set expected @>)

                [| 'A', 4, 10; 'B', 4, 8; 'C', 4, 10; 'D', 1, 4; 'E', 3, 8 |]
                |> testAreasAndPerimeters (nameof map1) map1

                [| 'X', 1, 4; 'X', 1, 4; 'X', 1, 4; 'X', 1, 4; 'O', 21, 36 |]
                |> testAreasAndPerimeters (nameof map2) map2

                [|
                    'R', 12, 18
                    'I', 4, 8
                    'C', 14, 28
                    'F', 10, 18
                    'V', 13, 20
                    'J', 11, 20
                    'C', 1, 4
                    'E', 13, 18
                    'I', 14, 22
                    'M', 5, 12
                    'S', 3, 8
                |]
                |> testAreasAndPerimeters (nameof map3) map3

                testCase "solve works with samples" (fun _ ->
                    test <@ PartOne.solve sample1 = 140 @>
                    test <@ PartOne.solve sample2 = 772 @>
                    test <@ PartOne.solve sample3 = 1930 @>)
            ]

            testList "PartTwo" [
                let testNumberOfSides mapName map expected =
                    testCase $"%s{mapName} regions have correct areas and perimeters" (fun _ ->
                        let result = map |> calculateRegions

                        let toCompare =
                            result |> Set.map (fun region -> region.PlantType, PartTwo.numberOfSides region)

                        test <@ toCompare = set expected @>)

                [| 'A', 4; 'B', 4; 'C', 8; 'D', 4; 'E', 4 |]
                |> testNumberOfSides (nameof map1) map1

                [|
                    'R', 10
                    'I', 4
                    'C', 22
                    'F', 12
                    'V', 10
                    'J', 12
                    'C', 4
                    'E', 8
                    'I', 16
                    'M', 6
                    'S', 6
                |]
                |> testNumberOfSides (nameof map3) map3

                [| 'E', 12; 'X', 4; 'X', 4 |] |> testNumberOfSides (nameof map4) map4
                [| 'A', 12; 'B', 4; 'B', 4 |] |> testNumberOfSides (nameof map5) map5

                testCase "solve works with samples" (fun _ ->
                    test <@ PartTwo.solve sample1 = 80 @>
                    test <@ PartTwo.solve sample2 = 436 @>
                    test <@ PartTwo.solve sample3 = 1206 @>
                    test <@ PartTwo.solve sample4 = 236 @>
                    test <@ PartTwo.solve sample5 = 368 @>)
            ]
        ]

let dayFns = {
    Tests = Test.all
    UtilTests = []
    PartOne = PartOne.solve >> int64
    PartTwo = PartTwo.solve >> int64
}
