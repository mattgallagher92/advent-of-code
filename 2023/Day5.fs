module Day5

open System

open FParsec

type Range = {
    DestinationRangeStart: int64
    SourceRangeStart: int64
    RangeLength: int64
}

type MapHeader = {
    SourceCategory: string
    DestinationCategory: string
}

type Map = {
    Header: MapHeader
    Ranges: Range list
}

let pSpace = pchar ' '

let pMapHeader =
    pipe2
        (many1 letter .>> pstring "-to-")
        (many1 letter .>> pSpace .>> pstring "map:")
        (fun letters letters' -> {
            SourceCategory = String(letters |> List.toArray)
            DestinationCategory = String(letters' |> List.toArray)
        })

let pRange =
    pipe3 (pint64 .>> pSpace) (pint64 .>> pSpace) pint64 (fun i j k -> {
        DestinationRangeStart = i
        SourceRangeStart = j
        RangeLength = k
    })

let pMap =
    pipe2
        pMapHeader
        (many (spaces >>? pRange))
        (fun header ranges -> { Header = header; Ranges = ranges })

let mapsInOrder unorderedMaps =
    List.unfold
        (fun sourceCategory ->
            unorderedMaps
            |> List.tryFind (fun m -> m.Header.SourceCategory = sourceCategory)
            |> Option.map (fun map -> map, map.Header.DestinationCategory))
        "seed"

let applyMap map (category, x) =
    if category = map.Header.SourceCategory then
        let applicableRange =
            map.Ranges
            |> List.tryFind (fun range ->
                range.SourceRangeStart <= x && x < range.SourceRangeStart + range.RangeLength)

        match applicableRange with
        | Some range -> (map.Header.DestinationCategory, x + range.DestinationRangeStart - range.SourceRangeStart)
        | None -> (map.Header.DestinationCategory, x)
    else
        (category, x)

module PartOne =

    type Almanac = {
        Seeds: int64 list
        Maps: Map list
    }

    let pSeeds = pstring "seeds:" >>. many (pSpace >>. pint64)

    let pAlmanac = pipe2 pSeeds (many (spaces1 >>? pMap)) (fun seeds maps -> { Seeds = seeds; Maps = maps })

    let solve text =

        let almanac = Parser.runAndUnwrap id pAlmanac text

        let applyAllMaps =
            almanac.Maps
            |> mapsInOrder
            |> List.map applyMap
            |> List.reduce (>>)

        almanac.Seeds
        |> List.map (fun i -> applyAllMaps ("seed", i))
        |> List.minBy snd

module PartTwo =

    type SeedRange =
        {
            SeedRangeStart: int64
            SeedRangeLength: int64
        }
        member this.Seeds =
            seq { this.SeedRangeStart .. (this.SeedRangeStart + this.SeedRangeLength - 1L) }

    type Almanac = {
        SeedRanges: SeedRange list
        Maps: Map list
    }

    let pSeeds =
        pstring "seeds:"
        >>. many
            (pipe2
                (pSpace >>. pint64)
                (pSpace >>. pint64)
                (fun i j -> { SeedRangeStart = i; SeedRangeLength = j }))

    let pAlmanac = pipe2 pSeeds (many (spaces1 >>? pMap)) (fun seeds maps -> { SeedRanges = seeds; Maps = maps })

    /// Adds ranges where source values map to the same destination value.
    let includeImplicitRanges map =

        let explicitRanges = map.Ranges

        let implicitRanges =

            let sortedExplicit =
                explicitRanges
                |> Seq.sortBy (fun r -> r.SourceRangeStart)
                |> Seq.toArray

            let before =

                let firstExplicitRangeStart = sortedExplicit |> Array.head |> fun r -> r.SourceRangeStart

                if firstExplicitRangeStart = 0L then
                    None
                else
                    Some {
                        SourceRangeStart = 0L
                        DestinationRangeStart = 0L
                        RangeLength = firstExplicitRangeStart
                    }

            let between =
                sortedExplicit
                |> Seq.pairwise
                |> Seq.map (fun (r1, r2) ->
                    let nextRangeStart = r1.SourceRangeStart + r1.RangeLength
                    if nextRangeStart < r2.SourceRangeStart then
                        Some {
                            SourceRangeStart = nextRangeStart
                            DestinationRangeStart = nextRangeStart
                            RangeLength = r2.SourceRangeStart - nextRangeStart
                        }
                    else
                        None)

            let after =

                let nextRangeStart =
                    sortedExplicit |> Array.last |> fun r -> r.SourceRangeStart + r.RangeLength

                Some {
                    SourceRangeStart = nextRangeStart
                    DestinationRangeStart = nextRangeStart
                    RangeLength = Int64.MaxValue - nextRangeStart
                }

            seq { before; yield! between; after } |> Seq.choose id

        { map with
              Ranges =
                explicitRanges
                |> Seq.append implicitRanges
                |> Seq.sortBy (fun r -> r.DestinationRangeStart)
                |> Seq.toList
        }

    type CategoryRange = {
        Category: string
        RStart: int64
        RLength: int64
    }

    let solve text =

        let almanac = Parser.runAndUnwrap id pAlmanac text

        let initialCategoryRanges =
            almanac.SeedRanges
            |> List.map (fun sr -> { Category = "seed"; RStart = sr.SeedRangeStart; RLength = sr.SeedRangeLength })

        // Including implicit ranges simplifies calculation.
        let maps = almanac.Maps |> List.map includeImplicitRanges |> mapsInOrder

        (initialCategoryRanges, maps)
        ||> List.fold (fun categoryRanges map ->

            let sortedRanges = map.Ranges |> List.sortBy  (fun range -> range.SourceRangeStart)

            categoryRanges
            |> List.collect (fun cr ->

                let cat, start = applyMap map (cr.Category, cr.RStart)

                let filteredRanges =
                    sortedRanges
                    |> List.filter (fun r ->
                        cr.RStart < r.SourceRangeStart && r.SourceRangeStart < cr.RStart + cr.RLength)

                let laterPartitions =
                    filteredRanges
                    |> List.choose (fun r ->
                        if cr.RStart < r.SourceRangeStart && r.SourceRangeStart < cr.RStart + cr.RLength then
                            Some {
                                Category = map.Header.DestinationCategory
                                RStart = r.DestinationRangeStart
                                RLength = Math.Min(r.RangeLength, cr.RStart + cr.RLength - r.SourceRangeStart)
                            }
                        else
                            None)

                let firstPartitionLength =
                    match filteredRanges |> List.tryHead with
                    | Some range -> range.SourceRangeStart - cr.RStart
                    | None -> cr.RLength

                { Category = cat; RStart = start; RLength = firstPartitionLength } :: laterPartitions))
        |> List.map (fun r -> r.RStart)
        |> List.min

