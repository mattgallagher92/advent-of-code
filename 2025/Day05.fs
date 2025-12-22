module Day05

type InclusiveRange = {
    Lower: int64
    Upper: int64
} with

    member this.Contains x = this.Lower <= x && x <= this.Upper
    member this.Size = this.Upper - this.Lower + 1L

module InclusiveRange =

    /// Minimum set of non-overlapping ranges that is equivalent to the given ranges.
    let combine (ranges: InclusiveRange array) =
        let orderedRanges = ranges |> Array.sortBy _.Lower

        ([| orderedRanges[0] |], orderedRanges)
        ||> Array.fold (fun combined nextRange ->
            // Because of ordering, earlier ranges cannot overlap.
            let last = combined |> Array.last
            let nextRange = nextRange
            let lowerContained = last.Contains nextRange.Lower
            let upperContained = last.Contains nextRange.Upper

            match lowerContained, upperContained with
            | false, false -> [| yield! combined; nextRange |]
            | false, true -> failwith "Ordering prohbits upper containment without lower containment."
            | true, false -> [|
                yield! combined[.. combined.Length - 2]
                { last with Upper = nextRange.Upper }
              |]
            | true, true -> combined)

let parse (lines: string array) =
    let emptyIx = lines |> Array.findIndex ((=) "")

    let ranges =
        lines[.. emptyIx - 1]
        |> Array.map (fun s ->
            s.Split('-')
            |> fun parts -> {
                Lower = int64 parts[0]
                Upper = int64 parts[1]
            })

    let available = lines[emptyIx + 1 ..] |> Array.map int64

    ranges, available

module PartOne =

    let solve (lines: string array) =
        let ranges, availabe = parse lines
        let combinedRanges = InclusiveRange.combine ranges

        // NOTE: Could be made more efficient by ordering available IDs and discarding earlier ranges in each iteration.
        availabe
        |> Array.countIf (fun a ->
            combinedRanges
            |> Array.tryFind (fun r -> a <= r.Upper)
            |> Option.map (fun r -> r.Lower <= a)
            |> Option.defaultValue false)

module PartTwo =

    let solve (lines: string array) =
        let ranges, _ = parse lines
        let combinedRanges = InclusiveRange.combine ranges

        combinedRanges |> Array.sumBy _.Size

module Test =

    open Expecto
    open Swensen.Unquote

    let all =
        testList "Day05" [
            let sampleInput = [| "3-5"; "10-14"; "16-20"; "12-18"; ""; "1"; "5"; "8"; "11"; "17"; "32" |]

            testList "PartOne" [
                testCase "solve works with sample input" (fun _ -> test <@ PartOne.solve sampleInput = 3 @>)
            ]

            testList "PartTwo" [
                testCase "solve works with sample input" (fun _ -> test <@ PartTwo.solve sampleInput = 14 @>)
            ]
        ]

let dayFns = {
    Tests = Test.all
    UtilTests = []
    PartOne = PartOne.solve >> int64
    PartTwo = PartTwo.solve >> int64
}
