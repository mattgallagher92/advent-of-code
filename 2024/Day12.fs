module Day12

module PartOne =

    // Plan:
    // - Scan through all plots, row-by-row, building up the following data for each plot:
    //   - For current plot: PlantType; RegionId; IsTopBoundary; IsLeftBoundary. Determine by checking plot above and plot to left.
    //   - For plot to left: IsRightBoundary. Also set for last plot in row.
    //   - For plot above: IsBottomBoundary. Also set for plots in last row.
    // - Group by region ID to get regions. Area is number of plots; perimeter is sum of Is*Boundary counts.
    let solve (lines: string array) = -1

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

            testList "PartOne" [
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
