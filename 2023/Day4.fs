module Day4

open System

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
            NumbersYouHave = Set numbersYouHave
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

