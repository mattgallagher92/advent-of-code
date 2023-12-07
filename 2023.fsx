open System

[<AutoOpen>]
module Util =
    [<RequireQualifiedAccess>]
    module Math =
        let integerPower base' exponent =
            if exponent < 0 then
                invalidArg (nameof(exponent)) $"exponent must be at least 0; given %i{exponent}"
            else
                Seq.replicate exponent base'
                |> Seq.fold (*) 1

    [<RequireQualifiedAccess>]
    module Parser =
        open FParsec
        let runAndUnwrap f parser line =
            match CharParsers.run parser line with
            | Success (x, _, _) -> f x
            | Failure (error, _, _) -> failwith $"Failed to parse line (%s{error}): %s{line}"

module Day1 =
    module PartOne =
        let solve lines =
            let toCalibrationValue line =
                line
                |> String.filter Char.IsDigit
                |> fun cs -> Int32.Parse $"%c{Seq.head cs}%c{Seq.last cs}"

            lines
            |> Seq.sumBy toCalibrationValue
            |> fun totalCalibration -> printfn $"totalCalibration is %i{totalCalibration}"

    module PartTwo =
        let solve lines =
            let toCalibrationValue (line: string) =
                let digitRepresentations =
                    [
                        "0", 0
                        "1", 1
                        "2", 2
                        "3", 3
                        "4", 4
                        "5", 5
                        "6", 6
                        "7", 7
                        "8", 8
                        "9", 9
                        "one", 1
                        "two", 2
                        "three", 3
                        "four", 4
                        "five", 5
                        "six", 6
                        "seven", 7
                        "eight", 8
                        "nine", 9
                    ]

                let indices =
                    digitRepresentations
                    |> List.map (fun (rep, i) -> i, line.IndexOf rep, line.LastIndexOf rep)

                let firstDigit =
                    indices
                    |> List.filter (fun (_, first, _) -> first >= 0)
                    |> List.minBy (fun (_, first, _) -> first)
                    |> fun (i, _, _) -> i

                let lastDigit =
                    indices
                    |> List.filter (fun (_, last, _) -> last >= 0)
                    |> List.maxBy (fun (_, _, last) -> last)
                    |> fun (i, _, _) -> i

                10 * firstDigit + lastDigit

            lines
            |> Seq.sumBy toCalibrationValue
            |> fun totalCalibration -> printfn $"totalCalibration is %i{totalCalibration}"

#r "nuget: FParsec"

module Day2 =
    open FParsec
    type Colour =
        | Red
        | Green
        | Blue

    type Set = {
        RedCount: int
        GreenCount: int
        BlueCount: int
    }

    module Set =
        let colourCount colour set =
            match colour with
            | Red -> set.RedCount
            | Green -> set.GreenCount
            | Blue -> set.BlueCount

    type Game =
        {
            GameId: int
            Sets: Set list
        }
        member this.MaxColourCount colour = this.Sets |> List.map (Set.colourCount colour) |> List.max
        member this.Power = this.MaxColourCount Red * this.MaxColourCount Green * this.MaxColourCount Blue

    let pGameId = pstring "Game " >>. pint32 .>> pstring ": "
    let pColour = stringReturn "red" Red <|> stringReturn "green" Green <|> stringReturn "blue" Blue
    let pColourCount = pint32 .>> pstring " " .>>. pColour
    let pSet = sepBy pColourCount (pstring ", ")
    let pGame = pGameId .>>. sepBy pSet (pstring "; ")
    let parse line =
        match CharParsers.run pGame line with
        | Success ((gameId, sets), _, _) ->
            let sets =
                sets
                |> List.map (fun colourCounts ->
                    let count colour =
                        colourCounts
                        |> List.tryFind (fun (_, c) -> c = colour)
                        |> Option.map fst
                        |> Option.defaultValue 0
                    {
                        RedCount = count Red
                        GreenCount = count Green
                        BlueCount = count Blue
                    })

            { GameId = gameId; Sets = sets }
        | Failure (error, _, _) ->
            failwith $"Failed to parse line (%s{error}): %s{line}"

    module PartOne =
        let solve lines =
            lines
            |> Seq.map parse
            |> Seq.filter (fun game ->
                game.MaxColourCount Red <= 12 &&
                game.MaxColourCount Green <= 13 &&
                game.MaxColourCount Blue <= 14)
            |> Seq.sumBy (fun game -> game.GameId)
            |> fun sumOfGameIds -> printfn $"Sum of game IDs: %i{sumOfGameIds}"

    module PartTwo =
        let solve lines =
            lines
            |> Seq.map parse
            |> Seq.sumBy (fun game -> game.Power)
            |> fun sumOfPowers -> printfn $"Sum of powers is %i{sumOfPowers}"

module Day3 =
    let matchingLocations predicate lines =
        lines
        |> Seq.indexed
        |> Seq.collect (fun (i, line) ->
            line
            |> Seq.indexed
            |> Seq.filter (snd >> predicate)
            |> Seq.map (fun (j, _) -> i, j))

    let numbersWithLocations lines =
        lines
        |> matchingLocations Char.IsDigit
        |> Seq.fold
            (fun (adjacentDigitLists, (lastI, lastJ)) (i,j) ->
                let adjacentDigitLists =
                    let digit = lines |> Seq.item i |> Seq.item j
                    if i = lastI && j = lastJ + 1 then
                        let digitsSoFar, previousNumbers =
                            match adjacentDigitLists with
                            | [] -> [], []
                            | soFar :: previous -> soFar, previous
                        ((digit, (i,j)) :: digitsSoFar) :: previousNumbers
                    else
                        [ (digit, (i,j)) ] :: adjacentDigitLists
                adjacentDigitLists, (i,j)
            )
            ([], (-1, 0))
        |> fst
        |> List.map (
            List.rev
            >> List.unzip
            >> (fun (digits, locations) -> digits |> List.toArray |> Int32.Parse, locations)
        )
        |> List.rev

    let locationsAdjacentTo (i, j) =
        seq {
            (i - 1, j - 1); (i - 1, j); (i - 1, j + 1)
            (i, j - 1);                 (i, j + 1)
            (i + 1, j - 1); (i + 1, j); (i + 1, j + 1)
        }

    module PartOne =
        let solve lines =
            let partNumbers =
                let locationsAdjacentToSymbols =
                    lines
                    |> matchingLocations (fun c -> Char.IsSymbol c || Char.IsPunctuation c && c <> '.')
                    |> Seq.collect locationsAdjacentTo
                    |> Set

                let isAdjacentToSymbol (_, locations) =
                    Set locations
                    |> Set.intersect locationsAdjacentToSymbols
                    |> Set.isEmpty
                    |> not

                lines
                |> numbersWithLocations
                |> List.filter isAdjacentToSymbol
                |> List.map fst

            List.sum partNumbers

    module PartTwo =
        let solve lines =
            let gearRatios =
                let numbersAroundEachAsterisk =
                    let numbersWithLocations = numbersWithLocations lines
                    lines
                    |> matchingLocations (fun c -> c = '*')
                    |> Seq.map (fun asteriskLocation ->
                        let locationsAdjacentToAsterisk = locationsAdjacentTo asteriskLocation |> Set
                        numbersWithLocations
                        |> List.filter (snd >> Set >> Set.intersect locationsAdjacentToAsterisk >> Set.isEmpty >> not)
                        |> List.map fst)

                numbersAroundEachAsterisk
                |> Seq.filter (fun numbers -> numbers.Length = 2)
                |> Seq.map (List.reduce (*))

            Seq.sum gearRatios

module Day4 =
    open FParsec

    type Card =
        {
            Number: int
            WinningNumbers: int Set
            NumbersYouHave: int Set
        }
        member this.MatchingNumberCount =
            Set.intersect this.WinningNumbers this.NumbersYouHave
            |> Set.count

    let pCardNumber = pstring "Card" >>. spaces1 >>. pint32 .>> pstring ":"
    // >>? results in lookahead behaviour, meaning that the '|' won't cause an error.
    // See http://www.quanttec.com/fparsec/users-guide/looking-ahead-and-backtracking.html
    let pNumbers = many (spaces1 >>? pint32)
    let pCard = pCardNumber .>>. pNumbers .>> pstring " |" .>>. pNumbers

    let parse line =
        match CharParsers.run pCard line with
        | Success (((cardNumber, winningNumbers), numbersYouHave), _, _) ->
            {
                Number = cardNumber
                WinningNumbers = Set winningNumbers
                NumbersYouHave =  Set numbersYouHave
            }
        | Failure (error, _, _) ->
            failwith $"Failed to parse line (%s{error}): %s{line}"

    module PartOne =
        let solve lines =
            lines
            |> Seq.map parse
            |> Seq.sumBy (fun card ->
                card.MatchingNumberCount
                |> fun count -> if count = 0 then 0 else Math.integerPower 2 (count - 1))

    module PartTwo =
        let solve lines =
            let contributions = Collections.Generic.Dictionary<int,int>()

            lines
            |> Seq.map parse
            // We can determine each card's contribution to the total by working out which copies it produces and using
            // the contributions of later cards. This means that if we go through the sequence backwards then we won't
            // have to keep track of the number of copies of each card, which should be simpler.
            |> Seq.rev
            |> Seq.sumBy (fun card ->
                let copyNumbers = Seq.init card.MatchingNumberCount (fun i -> card.Number + 1 + i)
                let contributionsFromCopies = copyNumbers |> Seq.sumBy (fun num -> contributions.Item num)
                let contribution = 1 + contributionsFromCopies

                contributions.Add(card.Number, contribution)

                contribution)
module Day5 =
    open FParsec

    module PartOne =
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

        type Almanac = {
            Seeds: int64 list
            Maps: Map list
        }

        let pSpace = pchar ' '

        let pSeeds = pstring "seeds:" >>. many (pSpace >>. pint64)

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

        let pFile = pipe2 pSeeds (many (spaces1 >>? pMap)) (fun seeds maps -> { Seeds = seeds; Maps = maps })

        let solve filePath =
            let almanac =
                match runParserOnFile pFile () filePath Text.Encoding.UTF8 with
                | Success (almanac, _, _) -> almanac
                | Failure (msg, _, _) -> failwith msg

            let mapsInOrder =
                List.unfold
                    (fun sourceCategory ->
                        almanac.Maps
                        |> List.tryFind (fun m -> m.Header.SourceCategory = sourceCategory)
                        |> Option.map (fun map -> map, map.Header.DestinationCategory))
                    "seed"

            let applyMap map (category, x) =
                if category = map.Header.SourceCategory then
                    let applicableRange =
                        map.Ranges
                        |> List.tryFind (fun range ->
                            range.SourceRangeStart <= x && x < range.SourceRangeStart + range.RangeLength)

                    let dest = map.Header.DestinationCategory
                    match applicableRange with
                    | Some range -> (dest, x + range.DestinationRangeStart - range.SourceRangeStart)
                    | None -> (dest, x)
                else
                    (category, x)

            let applyAllMaps =
                mapsInOrder
                |> List.map applyMap
                |> List.reduce (>>)

            almanac.Seeds
            |> List.map (fun i -> applyAllMaps ("seed", i))
            |> List.minBy snd

module Day6 =
    open FParsec

    module PartOne =
        let solve (lines: string array) =
            let numberOfChargeTimesThatExceedRecordDistance (timeAllowed, recordDistance) =
                // If charge time is c and time allowed is t, then the distance travelled is c(t-c). It beats the record
                // distance r if c(t-c) > r. Expressed another way: c^2 - tc + r < 0; or (c - t/2)^2 < (t^2)/4 - r.
                // From this expression it can be seen that for any value of c which satisfies this inequality, a
                // different value of c which is at least as close to t/2 as c will also satisfy the inequality. Thus,
                // if we find the lowest positive integer value of c for which the inequality holds, c' (for which
                // c' - t/2 will be negative), c' + 2 * (t/2 - c') = t - c' will be the largest positive integer value
                // of c for which the inequality holds. There are therefore (t - c') - c' + 1 = t - 2c' + 1
                // possibilities for c satisfying the inequality.
                Seq.init timeAllowed (fun i -> i * (timeAllowed - i) - recordDistance)
                |> Seq.findIndex (fun d -> d > 0)
                |> fun c -> timeAllowed - 2 * c + 1

            let times = lines.[0] |> Parser.runAndUnwrap id (pstring "Time:" >>. many (spaces1 >>. pint32))
            let distances = lines.[1] |> Parser.runAndUnwrap id (pstring "Distance:" >>. many (spaces1 >>. pint32))

            (times, distances)
            ||> List.zip
            |> List.map numberOfChargeTimesThatExceedRecordDistance
            |> List.reduce (*)

// FSI process has to run in same directory as this .fsx file for the relative path to work correctly.
"./day6input"
|> System.IO.File.ReadAllLines
|> Day6.PartOne.solve
