module Day3

open System

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

