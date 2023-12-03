open System

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
    module PartOne =
        let solve lines =
            let matchingLocations predicate =
                lines
                |> Seq.indexed
                |> Seq.collect (fun (i, line) ->
                    line
                    |> Seq.indexed
                    |> Seq.filter (snd >> predicate)
                    |> Seq.map (fun (j, _) -> i, j))

            let numbersWithLocations =
                matchingLocations Char.IsDigit
                |> Seq.fold
                    // TODO: rename symbols
                    (fun (numbersStrings, (lastI, lastJ)) (i,j) ->
                        let numberStrings =
                            let digit = lines |> Seq.item i |> Seq.item j
                            if i = lastI && j = lastJ + 1 then
                                let numberSoFar, previousNumbers =
                                    match numbersStrings with
                                    | [] -> [], []
                                    | soFar :: previous -> soFar, previous
                                ((digit, (i,j)) :: numberSoFar) :: previousNumbers
                            else
                                [ (digit, (i,j)) ] :: numbersStrings
                        numberStrings, (i,j)
                    )
                    ([], (-1, 0))
                |> fst
                |> List.map (
                    List.rev
                    >> List.unzip
                    >> (fun (digits, locations) -> digits |> List.toArray |> Int32.Parse, locations)
                )
                |> List.rev

            let partNumbers =
                let locationsAdjacentToSymbols =
                    matchingLocations (fun c -> Char.IsSymbol c || Char.IsPunctuation c && c <> '.')
                    |> Seq.collect (fun (i, j) -> seq {
                        (i - 1, j - 1); (i - 1, j); (i - 1, j + 1)
                        (i, j - 1);                 (i, j + 1)
                        (i + 1, j - 1); (i + 1, j); (i + 1, j + 1)
                    })
                    |> Set

                let isAdjacentToSymbol (_, locations) =
                    Set locations
                    |> Set.intersect locationsAdjacentToSymbols
                    |> Set.isEmpty
                    |> not

                numbersWithLocations
                |> List.filter isAdjacentToSymbol
                |> List.map fst

            List.sum partNumbers

// FSI process has to run in same directory as this .fsx file for the relative path to work correctly.
"./day3input"
|> System.IO.File.ReadAllLines
|> Day3.PartOne.solve
