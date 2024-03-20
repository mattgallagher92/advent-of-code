module Day2

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

