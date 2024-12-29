module Day12

module PartOne =

    // Plan:
    // - Repeat following until whole map is covered to find all regions.
    //   - Take left-most plot from top-most uncovered row.
    //   - Depth- or breadth-first search to find region of same plants (TEST).
    // - Area is number of plots in region.
    // - Find perimeter by either:
    //   1. Adding exterior perimeter to interior perimeters, which are exterior perimeters of contained regions.
    //     - First find perimeters of contained regions (recurse).
    //     - Find exterior perimeter by boundary tracing (TEST):
    //       - Start at left-most top-most contained plot.
    //       - Go right until the next plot to the right is not in the shape (trace a top edge).
    //       - Go down until the next plot below is not in the shape (trace a right edge).
    //       - Go left until the next plot to the left is not in the shape (trace a bottom edge).
    //       - Go up until the next plot above is not in the shape (trace a left edge).
    //       - Stop when back at start position.
    //     - Add perimeter contributions.
    //     - O(rp) where region size and p is perimeter length (for each plot on perimeter, check set inclusion for next plot)
    //   2. Summing perimeter contributions from every plot
    //     - Perimeter contribution from each plot is number of adjacent plots that are not in the region.
    //     - O(r^2) where r is region size (for each contained plot, checking set inclusion for each adjacent plot).
    // - TODO: how to find contained regions? (TEST)
    //
    // Alternative:
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
