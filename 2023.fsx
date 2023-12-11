#r "nuget: FParsec"

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

        /// Returns the greatest common divisor of a and b.
        let gcd a b =
            // Implements Euclid's algorithm.
            let rec inner a b = if b = 0UL then a else inner b (a % b)
            inner a b

        /// Returns the least common multiple of a and b.
        let lcm a b = (a * b) / gcd a b

    [<RequireQualifiedAccess>]
    module Parser =
        open FParsec
        let runAndUnwrap f parser line =
            match CharParsers.run parser line with
            | Success (x, _, _) -> f x
            | Failure (error, _, _) -> failwith $"Failed to parse line (%s{error}): %s{line}"

    [<RequireQualifiedAccess>]
    module Seq =
        let containsRepeats xs = Seq.length (Seq.distinct xs) < Seq.length xs

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

    let numberOfChargeTimesThatExceedRecordDistance (timeAllowed, recordDistance: int64) =
        // If charge time is c and time allowed is t, then the distance travelled is c(t-c). It beats the record
        // distance r if c(t-c) > r. Expressed another way: c^2 - tc + r < 0; or (c - t/2)^2 < (t^2)/4 - r.
        // From this expression it can be seen that for any value of c which satisfies this inequality, a
        // different value of c which is at least as close to t/2 as c will also satisfy the inequality. Thus,
        // if we find the lowest positive integer value of c for which the inequality holds, c' (for which
        // c' - t/2 will be negative), c' + 2 * (t/2 - c') = t - c' will be the largest positive integer value
        // of c for which the inequality holds. There are therefore (t - c') - c' + 1 = t - 2c' + 1
        // possibilities for c satisfying the inequality.
        Seq.init timeAllowed (fun i -> (int64 i) * ((int64 timeAllowed) - (int64 i)) - recordDistance)
        |> Seq.findIndex (fun d -> d > 0)
        |> fun c -> timeAllowed - 2 * c + 1

    module PartOne =
        let solve (lines: string array) =
            let times = lines.[0] |> Parser.runAndUnwrap id (pstring "Time:" >>. many (spaces1 >>. pint32))
            let distances = lines.[1] |> Parser.runAndUnwrap id (pstring "Distance:" >>. many (spaces1 >>. pint64))

            (times, distances)
            ||> List.zip
            |> List.map numberOfChargeTimesThatExceedRecordDistance
            |> List.reduce (*)

    module PartTwo =
        let solve (lines: string array) =
            let timeAllowed = lines.[0] |> Seq.filter Char.IsDigit |> Seq.toArray |> String |> Int32.Parse
            let recordDistance = lines.[1] |> Seq.filter Char.IsDigit |> Seq.toArray |> String |> Int64.Parse

            numberOfChargeTimesThatExceedRecordDistance (timeAllowed, recordDistance)

module Day7 =

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

module Day8 =

    type Direction =
        | Left
        | Right

    type Node = {
        Label: char * char * char
        LeftLabel: char * char * char
        RightLabel: char * char * char
    }

    let parse lines =

        let directions =
            lines
            |> Array.item 0
            |> Seq.map (function
                | 'L' -> Left
                | 'R' -> Right
                | c -> failwith $"Invalid direction $c{c}")

        let nodes =
            lines
            |> Seq.skip 2
            |> Seq.map (fun line ->
                match line |> Seq.toList with
                | c1 :: c2 :: c3 :: ' ' :: '=' :: ' ' :: '(' :: l1 :: l2 :: l3 :: ',' :: ' ' :: r1 :: r2 :: r3 :: [ ')' ] ->
                    {
                        Label = c1, c2, c3
                        LeftLabel = l1, l2, l3
                        RightLabel = r1, r2, r3
                    }
                | _ -> failwith $"Invalid line: %s{line}")

        directions |> Seq.toArray, nodes |> Seq.toArray

    module PartOne =

        let solve lines =

            let directions, nodes = parse lines

            let steps =
                let lookup = nodes |> Seq.map (fun n -> n.Label, n) |> dict
                let direction i = Array.item (i % directions.Length) directions
                Array.unfold
                    (fun (i, node) ->
                        match node.Label, direction i with
                        | ('Z', 'Z', 'Z'), _ -> None
                        | _, Left -> let n = lookup.Item node.LeftLabel in Some (n, (i + 1, n))
                        | _, Right -> let n = lookup.Item node.RightLabel in Some (n, (i + 1, n)))
                    (0, lookup.Item ('A', 'A', 'A'))

            steps.Length

    module PartTwo =

        type PathSignature = {
            /// The number of steps before the cycle starts.
            PreCycleLength: int
            /// The indices of steps before the cycle that correspond to nodes whose label ends in Z.
            PreCycleZIndices: int array
            /// The number of steps in the cycle.
            CycleLength: int
            /// The indices, relative to the cycle start, of steps within the cycle that correspond to nodes whose label
            /// ends in Z.
            CycleZIndices: int array
        }

        let solve lines =

            let directions, nodes = parse lines
            let nodeLookup = nodes |> Seq.map (fun n -> n.Label, n) |> dict

            let startNodes =
                nodes
                |> Array.filter (fun n -> match n.Label with _, _, 'A' -> true | _ -> false)

            /// For a given node, returns the nodes visited by starting there and following directions in order. Returns
            /// the last such node separately.
            let journeyLookup =
                let journey startNode =
                    let nodesVisited =
                        directions
                        |> Array.scan
                            (fun n d ->
                                match d with
                                | Left -> nodeLookup.Item n.LeftLabel
                                | Right -> nodeLookup.Item n.RightLabel)
                            startNode
                    nodesVisited.[0 .. (nodesVisited.Length - 2)], nodesVisited |> Array.last

                nodes
                |> Array.map (fun n -> n, journey n)
                |> dict

            // Keep repeating a journey (following all directions through once) until we end at a node that was the
            // start of a previous journey. Returns the nodes at the start of those journeys, and the node that is
            // repeated first.
            let journeyStartsUntilFirstRepeat startNode =

                let starts =
                    ([| startNode |], Seq.initInfinite ignore)
                    ||> Seq.scan
                        (fun previousStarts ()  ->
                            if Seq.containsRepeats previousStarts then
                                previousStarts
                            else
                                let previous = previousStarts |> Array.last
                                let _, nextStart = journeyLookup.Item previous
                                Array.append previousStarts [| nextStart |])
                    |> Seq.skipWhile (not << Seq.containsRepeats)
                    |> Seq.head

                starts |> Array.removeAt (starts.Length - 1), starts |> Array.last

            /// Returns the steps that are before a cycle starts, and a cycle of steps starting immediately afterwards
            /// that repeats forever.
            let decomposeFrom startNode =

                let journeyStarts, cycleStart = journeyStartsUntilFirstRepeat startNode

                let cycleStartIndex = journeyStarts |> Array.findIndex ((=) cycleStart)
                let preCycle =
                    journeyStarts
                    |> Array.take cycleStartIndex
                    |> Array.collect (fun n -> journeyLookup.Item n |> fst)
                let cycle =
                    journeyStarts
                    |> Array.skip cycleStartIndex
                    |> Array.collect (fun n -> journeyLookup.Item n |> fst)

                preCycle, cycle

            let pathSignatures =
                let isZNode n = match n.Label with _, _, 'Z' -> true | _ -> false

                startNodes
                |> Array.map (fun startNode ->
                    let preCycle, cycle = decomposeFrom startNode
                    {
                        PreCycleLength = preCycle.Length
                        PreCycleZIndices =
                            preCycle
                            |> Array.mapi (fun i n -> i, isZNode n)
                            |> Array.filter snd
                            |> Array.map fst
                        CycleLength = cycle.Length
                        CycleZIndices =
                            cycle
                            |> Array.mapi (fun i n -> i, isZNode n)
                            |> Array.filter snd
                            |> Array.map fst
                    })

            // TODO: implement in generic way.
            // This is a hack that makes use of some observations I've made about the particular input I have. It's not
            // sufficiently generic to handle every possible input of this type.
            //
            // The observations that I've made use of are:
            // - There are no pre-cycle z-indices
            // - The pre-cycles are all the same length
            // - There is only one cycle z-index, which is pre-cycle length away from the end of the cycle.
            //
            // As a result, any step index that is a z-index of a path must be divisible by that path signature's cycle
            // length. So a step index that is a z-index of all paths is divisible by all cycle lengths. The first such
            // step index is the least common multiple of the cycle lengths.
            pathSignatures
            |> Array.map (fun i -> i.CycleLength |> Convert.ToUInt64)
            |> Array.reduce Math.lcm

module Day9 =
    open FParsec

    let pLine: Parser<int32 list, unit> = sepBy pint32 (pchar ' ')

    let solve extrapolate lines =

        let histories =
            lines
            |> Array.map (Parser.runAndUnwrap List.toArray pLine)

        let differences numbers =
            numbers
            |> Seq.pairwise
            |> Seq.map (fun (i, j) -> j - i)

        // This doesn't include the final sequence of zeroes that are in the puzzle description; it seems easier to
        // work without them. It does include the input as the first element.
        let successiveDifferences ns =
            let differences =
                Array.unfold
                    (fun numbers ->
                        let diffs = differences numbers |> Seq.toArray
                        if diffs |> Array.forall ((=) 0) then
                            None
                        else
                            Some (diffs, diffs)
                    )
                    ns

            Array.append [| ns |] differences

        histories
        |> Array.map successiveDifferences
        |> Array.map extrapolate
        |> Array.sum

    module PartOne =

        let extrapolate successiveDifferences =
            (successiveDifferences |> Array.map Array.last, 0)
            ||> Array.foldBack (fun last deeperLast -> last + deeperLast)

        let solve = solve extrapolate

    module PartTwo =

        let extrapolate successiveDifferences =
            (successiveDifferences |> Array.map Array.head, 0)
            ||> Array.foldBack (fun first deeperFirst -> first - deeperFirst)

        let solve = solve extrapolate

// FSI process has to run in same directory as this .fsx file for the relative path to work correctly.
"./day9input"
|> System.IO.File.ReadAllLines
|> Day9.PartTwo.solve
