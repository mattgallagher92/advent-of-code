module Day12

let parse (lines: string array) =
    lines |> Array.map Seq.toArray |> array2D

type CompletePlotData = {|
    Coordinates: int * int
    PlantType: char
    RegionId: int * int
    IsTopBoundary: bool
    IsLeftBoundary: bool
    IsRightBoundary: bool
    IsBottomBoundary: bool
|}

type Direction =
    | Up
    | Down
    | Left
    | Right

type Region = {
    RegionId: int * int
    Plots: CompletePlotData array
} with

    member this.Area = this.Plots.Length

    member this.Perimeter() =
        this.Plots
        |> Array.sumBy (fun p ->
            [|
                if p.IsTopBoundary then
                    1
                if p.IsBottomBoundary then
                    1
                if p.IsLeftBoundary then
                    1
                if p.IsRightBoundary then
                    1
            |]
            |> Array.sum)

    member this.NumberOfCorners1() =
        let coordinateSet =
            this.Plots |> Array.map _.Coordinates |> System.Collections.Generic.HashSet

        this.Plots
        |> Array.sumBy (fun p ->
            [|
                // Inner corners.
                if p.IsTopBoundary && p.IsLeftBoundary then
                    1
                if p.IsTopBoundary && p.IsRightBoundary then
                    1
                if p.IsBottomBoundary && p.IsLeftBoundary then
                    1
                if p.IsBottomBoundary && p.IsRightBoundary then
                    1

                // Outer corners.
                let r, c = p.Coordinates

                if p.IsTopBoundary && not p.IsLeftBoundary && coordinateSet.Contains(r - 1, c - 1) then
                    1

                if p.IsTopBoundary && not p.IsRightBoundary && coordinateSet.Contains(r - 1, c + 1) then
                    1

                if
                    p.IsBottomBoundary
                    && not p.IsLeftBoundary
                    && coordinateSet.Contains(r + 1, c - 1)
                then
                    1

                if
                    p.IsBottomBoundary
                    && not p.IsRightBoundary
                    && coordinateSet.Contains(r + 1, c + 1)
                then
                    1
            |]
            |> Array.sum)

    member this.NumberOfCorners2() =
        let coordinateSet =
            this.Plots |> Array.map _.Coordinates |> System.Collections.Generic.HashSet

        let isIn coords = coordinateSet.Contains coords
        let isNotIn = not << isIn

        coordinateSet
        |> Seq.sumBy (fun (r, c) ->
            let u, d, l, ri = (r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)
            let ul, ur, dl, dr = (r - 1, c - 1), (r - 1, c + 1), (r + 1, c - 1), (r + 1, c + 1)

            [|
                if u |> isNotIn && l |> isNotIn then
                    1
                if u |> isNotIn && ri |> isNotIn then
                    1
                if d |> isNotIn && l |> isNotIn then
                    1
                if d |> isNotIn && ri |> isNotIn then
                    1

                if u |> isIn && l |> isIn && ul |> isNotIn then
                    1
                if u |> isIn && ri |> isIn && ur |> isNotIn then
                    1
                if d |> isIn && l |> isIn && dl |> isNotIn then
                    1
                if d |> isIn && ri |> isIn && dr |> isNotIn then
                    1
            |]
            |> Array.sum)

    member this.NumberOfSides() =
        let distinctSides filter sortBy consecutivePairIsPartOfSameSide (filteredPlots: CompletePlotData array) =
            filteredPlots
            |> Array.filter filter
            |> Array.map _.Coordinates
            |> Array.sortBy sortBy
            |> Array.pairwise
            |> fun pairs -> 1, pairs
            ||> Array.fold (fun sideCount pair ->
                if consecutivePairIsPartOfSameSide pair then
                    sideCount
                else
                    sideCount + 1)

        [|
            let sameRowNextCol ((r1, c1), (r2, c2)) = r2 = r1 && c2 = c1 + 1
            let sameColNextRow ((r1, c1), (r2, c2)) = c2 = c1 && r2 = r1 + 1

            (fun (p: CompletePlotData) -> p.IsTopBoundary), id, sameRowNextCol
            (fun (p: CompletePlotData) -> p.IsBottomBoundary), id, sameRowNextCol
            (fun (p: CompletePlotData) -> p.IsLeftBoundary), (fun (r, c) -> c, r), sameColNextRow
            (fun (p: CompletePlotData) -> p.IsRightBoundary), (fun (r, c) -> c, r), sameColNextRow
        |]
        |> Array.sumBy (fun (filter, sortBy, sameSide) -> this.Plots |> distinctSides filter sortBy sameSide)

    member this.NumberOfSides2() =
        let perimeterObjects =
            this.Plots
            |> Array.collect (fun p ->
                [|
                    if p.IsTopBoundary then
                        Up
                    if p.IsBottomBoundary then
                        Down
                    if p.IsLeftBoundary then
                        Left
                    if p.IsRightBoundary then
                        Right
                |]
                |> Array.map (fun d -> p.Coordinates, d))
            |> System.Collections.Generic.HashSet

        let mutable sides = 0

        while perimeterObjects.Count > 0 do
            let pos, d = perimeterObjects |> Seq.head
            perimeterObjects.Remove(pos, d) |> ignore
            sides <- sides + 1

            let perp1 (r, c) =
                match d with
                | Right -> r - 1, c
                | Down -> r, c + 1
                | Left -> r + 1, c
                | Up -> r, c - 1

            let mutable next = perp1 pos

            while perimeterObjects.Contains(next, d) do
                perimeterObjects.Remove(next, d) |> ignore
                next <- perp1 next

            let perp2 (r, c) =
                match d with
                | Left -> r - 1, c
                | Up -> r, c + 1
                | Right -> r + 1, c
                | Down -> r, c - 1

            let mutable next = perp2 pos

            while perimeterObjects.Contains(next, d) do
                perimeterObjects.Remove(next, d) |> ignore
                next <- perp2 next

        sides

type private PreviousPlotData = {|
    Coordinates: int * int
    PlantType: char
    RegionId: int * int
    IsTopBoundary: bool
    IsLeftBoundary: bool
|}

type private EarlierPlotData = {|
    Coordinates: int * int
    PlantType: char
    RegionId: int * int
    IsTopBoundary: bool
    IsLeftBoundary: bool
    IsRightBoundary: bool
|}

type private FoldState = {
    Previous: PreviousPlotData
    Earlier: EarlierPlotData array
    Complete: CompletePlotData array
    RegionIdUpdates: ((int * int) * (int * int)) list
}

let calculateRegions map =
    if map |> Array2D.length2 < 2 then
        failwith "Code assumes map has width at least 2"

    let initialState =
        (map, 0, 0)
        |||> Array2D.get
        |> fun plant -> {
            Previous = {|
                Coordinates = 0, 0
                PlantType = plant
                RegionId = 0, 0
                IsTopBoundary = true
                IsLeftBoundary = true
            |}
            Earlier = [||]
            Complete = [||]
            RegionIdUpdates = []
        }

    // Scan through all plots, row-by-row, building up the following data for each plot:
    //   - For current plot: PlantType; RegionId; IsTopBoundary; IsLeftBoundary. Determine by checking plot above and plot to left.
    //   - For plot to left: IsRightBoundary. Also set for last plot in row.
    //   - For plot above: IsBottomBoundary. Also set for plots in last row.
    (initialState, map |> Array2D.indexed |> Array.tail)
    ||> Array.fold (fun state ((r, c), plant) ->
        let abovePlant = (r - 1, c) ||> Array2D.tryGet map
        let leftPlant = (r, c - 1) ||> Array2D.tryGet map

        let isTopBoundary =
            abovePlant |> Option.map ((<>) plant) |> Option.defaultValue true

        let isLeftBoundary =
            leftPlant |> Option.map ((<>) plant) |> Option.defaultValue true

        let earlierAbove, otherEarlier =
            state.Earlier
            |> Array.partition (fun e -> e.Coordinates = (r - 1, c))
            |> Pair.mapFst Array.tryHead

        let newRegionId, regionIdToUpdate =
            match isTopBoundary, isLeftBoundary with
            | true, true -> (r, c), None
            | true, false -> state.Previous.RegionId, None
            // Since width is at least 2, earlierAbove is guaranteed to be Some if isTopBoundary is false.
            | false, true -> earlierAbove.Value.RegionId, None
            | false, false ->
                let above, left = earlierAbove.Value.RegionId, state.Previous.RegionId
                let new' = min above left
                let toUpdate = if above <> left then Some(max above left) else None
                new', toUpdate

        {
            Previous = {|
                Coordinates = r, c
                PlantType = plant
                RegionId = newRegionId
                IsTopBoundary = isTopBoundary
                IsLeftBoundary = isLeftBoundary
            |}
            Earlier = [|
                {| state.Previous with IsRightBoundary = isLeftBoundary |}
                yield! otherEarlier
            |]
            Complete =
                earlierAbove
                |> Option.map (fun earlierAbove -> [|
                    {| earlierAbove with IsBottomBoundary = isTopBoundary |}
                    yield! state.Complete
                |])
                |> Option.defaultValue state.Complete
            RegionIdUpdates =
                match regionIdToUpdate with
                | Some rId -> (rId, newRegionId) :: state.RegionIdUpdates
                | None -> state.RegionIdUpdates
        })
    |> fun state ->
        let distinctUpdates =
            state.RegionIdUpdates
            |> List.toArray
            // Reverse before and after distinct to keep later elements if there are duplicates.
            |> Array.rev
            |> Array.distinct
            |> Array.rev

        let updateRegionId (data: CompletePlotData) =
            (data, distinctUpdates)
            ||> Array.fold (fun c (regionIdToUpdate, newRegionId) ->
                if c.RegionId = regionIdToUpdate then
                    {| c with RegionId = newRegionId |}
                else
                    c)

        (state.Complete, state.Earlier)
        // Earlier is plots on bottom row except bottom-right.
        ||> Array.fold (fun complete earlier -> [| {| earlier with IsBottomBoundary = true |}; yield! complete |])
        |> fun rest -> [|
            {|
                // Previous is bottom-right plot
                state.Previous with
                    IsRightBoundary = true
                    IsBottomBoundary = true
            |}
            yield! rest
        |]
        |> Array.map updateRegionId
    // - Group by region ID to get regions.
    |> Array.groupBy _.RegionId
    |> Array.map (fun (regionId, plots) -> { RegionId = regionId; Plots = plots })

module PartOne =

    let fencingPrice (region: Region) = region.Area * region.Perimeter()

    let solve lines =
        lines |> parse |> calculateRegions |> Array.sumBy fencingPrice

module PartTwo =

    let bulkDiscountPrice (region: Region) = region.Area * region.NumberOfSides()

    let solve (lines: string array) =
        lines |> parse |> calculateRegions |> Array.sumBy bulkDiscountPrice

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

                        test
                            <@
                                result
                                |> Array.map (fun region -> region.Plots[0].PlantType, region.Area, region.Perimeter())
                                |> Array.sort = Array.sort expected
                            @>)

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

                        test
                            <@
                                result
                                |> Array.map (fun region -> region.Plots[0].PlantType, region.NumberOfSides2())
                                |> Array.sort = Array.sort expected
                            @>)

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
