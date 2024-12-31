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

    let solve (lines: string array) = -1

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


            let map1 = parse sample1
            let map2 = parse sample2
            let map3 = parse sample3

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
                testCase "solve works with sample input" (fun _ -> test <@ PartTwo.solve sample1 = -1 @>)
            ]
        ]

let dayFns = {
    Tests = Test.all
    UtilTests = []
    PartOne = PartOne.solve >> int64
    PartTwo = PartTwo.solve >> int64
}
