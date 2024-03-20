module Day7

open System

type Label =
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace

type Type' =
    | HighCard
    | OnePair
    | TwoPair
    | ThreeOfAKind
    | FullHouse
    | FourOfAKind
    | FiveOfAKind

let parse (line: string) =
    let parts = line.Split(' ')
    let hand =
        let labels = parts.[0]
        let parseLabel i =
            labels
            |> Seq.item i
            |> function
                | '2' -> Two
                | '3' -> Three
                | '4' -> Four
                | '5' -> Five
                | '6' -> Six
                | '7' -> Seven
                | '8' -> Eight
                | '9' -> Nine
                | 'T' -> Ten
                | 'J' -> Jack
                | 'Q' -> Queen
                | 'K' -> King
                | 'A' -> Ace
                | c -> failwith $"Invalid label char '%c{c}'"
        parseLabel 0, parseLabel 1, parseLabel 2, parseLabel 3, parseLabel 4
    let bid = parts.[1] |> Int32.Parse
    hand, bid

let solve classify rank lines =
    lines
    |> Array.map parse
    |> Array.sortBy (fun (l1, l2, l3, l4, l5 as hand, _) ->
        classify hand, (rank l1, rank l2, rank l3, rank l4, rank l5))
    |> Array.mapi (fun i (_, bid) -> bid * (i + 1))
    |> Array.sum

module PartOne =

    let classify (label1, label2, label3, label4, label5) =
        let labels = [ label1; label2; label3; label4; label5 ]
        let labelCounts = labels |> List.map (fun label -> labels |> List.filter ((=) label) |> List.length)
        let hasOfAKind i = labelCounts |> List.contains i
        let numberOfPairs = labelCounts |> List.filter ((=) 2) |> List.length |> fun i -> i / 2
        match hasOfAKind 5, hasOfAKind 4, hasOfAKind 3, numberOfPairs with
        | true, _, _, _ -> FiveOfAKind
        | false, true, _, _ -> FourOfAKind
        | false, false, true, 1 -> FullHouse
        | false, false, true, 0 -> ThreeOfAKind
        | false, false, false, 2 -> TwoPair
        | false, false, false, 1 -> OnePair
        | false, false, false, 0 -> HighCard
        | _, _, _, _ -> failwith $"Invalid labels %A{labels}"

    let solve = solve classify id

module PartTwo =

    let rank =
        function
        | Jack -> 1
        | Two -> 2
        | Three -> 3
        | Four -> 4
        | Five -> 5
        | Six -> 6
        | Seven -> 7
        | Eight -> 8
        | Nine -> 9
        | Ten -> 10
        | Queen -> 11
        | King -> 12
        | Ace -> 13

    let classify (label1, label2, label3, label4, label5) =
        let labels = [ label1; label2; label3; label4; label5 ]
        let nonJackLabels = labels |> List.filter ((<>) Jack)
        let nonJackLabelCounts =
            nonJackLabels |> List.map (fun label -> nonJackLabels |> List.filter ((=) label) |> List.length)

        let hasOfAKind i = nonJackLabelCounts |> List.contains i
        let numberOfPairs = nonJackLabelCounts |> List.filter ((=) 2) |> List.length |> fun i -> i / 2
        let numberOfJacks = 5 - List.length nonJackLabels

        match hasOfAKind 5, hasOfAKind 4, hasOfAKind 3, numberOfPairs, numberOfJacks with
        | true, _, _, _, _ -> FiveOfAKind
        | false, true, _, _, 1 -> FiveOfAKind
        | false, true, _, _, 0 -> FourOfAKind
        | false, false, true, 1, _ -> FullHouse
        | false, false, true, 0, 2 -> FiveOfAKind
        | false, false, true, 0, 1 -> FourOfAKind
        | false, false, true, 0, 0 -> ThreeOfAKind
        | false, false, false, 2, 1 -> FullHouse
        | false, false, false, 2, 0 -> TwoPair
        | false, false, false, 1, 3 -> FiveOfAKind
        | false, false, false, 1, 2 -> FourOfAKind
        | false, false, false, 1, 1 -> ThreeOfAKind
        | false, false, false, 1, 0 -> OnePair
        | false, false, false, 0, 5 -> FiveOfAKind
        | false, false, false, 0, 4 -> FiveOfAKind
        | false, false, false, 0, 3 -> FourOfAKind
        | false, false, false, 0, 2 -> ThreeOfAKind
        | false, false, false, 0, 1 -> OnePair
        | false, false, false, 0, 0 -> HighCard
        | _, _, _, _, _ -> failwith $"Invalid labels %A{nonJackLabels}"

    let solve = solve classify rank

