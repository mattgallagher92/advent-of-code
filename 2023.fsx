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

        /// Returns a sequence up to and including the first element of the input sequence that matches the predicate,
        /// then no more. If there are no matching elements in the input sequence, then all elements will be returned.
        // Implementation inspired by https://stackoverflow.com/a/12564899
        // TODO: add tests.
        let takeUntilFirstMatch predicate (s:seq<_>) =
            let rec loop (en:System.Collections.Generic.IEnumerator<_>) = seq {
                if en.MoveNext() then
                    yield en.Current
                    if not (predicate en.Current) then
                        yield! loop en
            }

            seq {
                use en = s.GetEnumerator()
                yield! loop en
            }

    type Collections.Generic.IDictionary<'a, 'b> with

        member this.TryGet k =
            match this.TryGetValue k with
            | true, v -> Some v
            | false, _ -> None

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

module Day10 =
    open System.Collections.Generic

    type Direction =
        | North
        | South
        | East
        | West

    module Direction =

        let opposite =
            function
            | North -> South
            | South -> North
            | East -> West
            | West -> East

    type Tile =
        | Pipe of Direction * Direction
        | Ground
        | StartingPosition

    let pTile =
        function
        | '|' -> Pipe (North, South)
        | '-' -> Pipe (East, West)
        | 'L' -> Pipe (North, East)
        | 'J' -> Pipe (North, West)
        | '7' -> Pipe (South, West)
        | 'F' -> Pipe (South, East)
        | '.' -> Ground
        | 'S' -> StartingPosition
        | c -> failwith $"Invalid tile char: '%c{c}"

    [<RequireQualifiedAccess>]
    module Tile =

        /// The directions that the pipe on the tile (if any) exits to.
        let exitDirections tile =
            match tile with
            | Pipe (a, b) -> [| a; b |]
            | Ground
            | StartingPosition -> [||]

        /// Whether the pipe on the tile (if any) has an exit in the given direction.
        let exits direction tile = tile |> exitDirections |> Array.contains direction

    /// Parses lines into an array of pairs, whose first elements are coordinates and second elements are the contents
    /// of the tile at those coordinates.
    let parse lines =
        lines
        |> Array.mapi (fun i line -> line |> Seq.mapi (fun j c -> (i, j), pTile c) |> Seq.toArray)
        |> Array.collect id

    /// Returns the coordinates of the starting tile, and a dictionary from coordinates to the tile contents. The value
    /// corresponding to the starting coordinates is the pipe that is calculated to be present there, not
    /// StartingPosition.
    let calculateStartCoordsAndTileLookup tiles =
        let startCoords = tiles |> Array.find (snd >> (=) StartingPosition) |> fst
        let lookup = dict tiles

        let startPipe =
            let exitDirections = [
                let exits direction coords =
                    coords |> lookup.TryGet |> Option.map (Tile.exits direction) |> Option.defaultValue false

                let i, j = startCoords

                if (i - 1, j) |> exits South then North
                if (i + 1, j) |> exits North then South
                if (i, j + 1) |> exits West then East
                if (i, j - 1) |> exits East then West
            ]

            Pipe (exitDirections.[0], exitDirections.[1])

        startCoords, dict (Array.append tiles [| startCoords, startPipe |])

    /// Returns a sequence of pairs whose first element represents a distance from the starting position in the main
    /// loop, and whose second element is the set of the coordinates which correspond to tiles at that distance.
    let calculateCoordsByDistanceSeq startCoords (tileLookup: IDictionary<int * int, Tile>) =

        let startDirections = tileLookup.Item startCoords |> Tile.exitDirections

        let distanceGoingIn startDirection =

            ((startCoords, startDirection), Seq.initInfinite ignore)
            ||> Seq.scan (fun ((i, j), inDirection) _ ->
                let outDirection =
                    tileLookup.Item (i, j)
                    |> Tile.exitDirections
                    |> Array.find ((<>) inDirection)

                let nextCoords =
                    match outDirection with
                    | North -> (i - 1, j)
                    | South -> (i + 1, j)
                    | East -> (i, j + 1)
                    | West -> (i, j - 1)

                (nextCoords, outDirection |> Direction.opposite))
            |> Seq.map fst

        (distanceGoingIn startDirections.[0], distanceGoingIn startDirections.[1])
        ||> Seq.zip
        |> Seq.indexed
        // The paths are guaranteed to hit the midpoint of the loop at the same time because the length of the main loop
        // is even: the number of steps North equals the number of steps South, and likewise for East and West.
        |> Seq.takeUntilFirstMatch (fun (distance, (cs1, cs2)) -> distance <> 0 && cs1 = cs2)
        |> Seq.map (fun (distance, (cs1, cs2)) -> distance, Set (seq { cs1; cs2 }))

    module PartOne =

        let solve lines =
            parse lines
            |> calculateStartCoordsAndTileLookup
            ||> calculateCoordsByDistanceSeq
            |> Seq.last
            |> fst

    module PartTwo =

        // Consider the vertices of the square tiles that make up the grid.
        // The tiles which are not inside the loop are precisely those which have a vertex that can be reached by a path
        // starting from a vertex on the edge of the grid that traverses along sides that aren't crossed by the main
        // loop (which is to say that the main loop does not go directly between the two tiles that share the side).
        // To find all such vertices this function starts with the vertices at the edge of grid and iteratively adds
        // adjacent vertices that can be reached without crossing the main loop, until no new vertices are added.
        let solve lines =

            let startTileCoords, tileLookup = parse lines |> calculateStartCoordsAndTileLookup

            let rowCount = tileLookup.Keys |> Seq.map fst |> Seq.max
            let columnCount = tileLookup.Keys |> Seq.map snd |> Seq.max

            let mainLoopTileCoords =
                (startTileCoords, tileLookup)
                ||> calculateCoordsByDistanceSeq
                |> Seq.map snd
                |> Set.unionMany

            let accessibleAdjacentVertices (i, j) = seq {
                // Vertex (i, j) is a vertex of the tiles (i-1, j-1), (i-1, j), (i, j-1), (i, j) that are present in the
                // grid (edge vertices won't have four such tiles around them).
                // The adjacent vertices are (i-1, j), (i, j-1), (i, j+1) and (i+1, j).
                //                            Vertex (i-1, j)
                //                                 |
                //                                 v
                //                    x------------x------------x
                //                    | (i-1, j-1) | (i-1, j)   |
                // Vertex (i, j-1) -> x------------x------------x <- Vertex (i, j+1)
                //                    | (i, j-1)   | (i, j)     |
                //                    x------------x------------x
                //                                 ^
                //                                 |
                //                           Vertex (i+1, j)
                let tileI'J' = (i-1, j-1) |> fun cs -> cs, tileLookup.TryGet cs
                let tileI'J = (i-1, j) |> fun cs -> cs, tileLookup.TryGet cs
                let tileIJ' = (i, j-1) |> fun cs -> cs, tileLookup.TryGet cs
                let tileIJ = (i, j) |> fun cs -> cs, tileLookup.TryGet cs

                let connectedByMainLoop (((cs1, t1), d1), ((cs2, t2), d2)) =
                    mainLoopTileCoords |> Set.contains cs1
                    && mainLoopTileCoords |> Set.contains cs2
                    &&
                        (t1, t2)
                        ||> Option.map2 (fun t1 t2 -> t1 |> Tile.exits d1 && t2 |> Tile.exits d2)
                        |> Option.defaultValue false

                // Vertex (i-1, j) can be reached if there is no East-West pipe between tiles (i-1, j-1) and (i-1, j).
                if i > 0 && not (connectedByMainLoop ((tileI'J', East), (tileI'J, West))) then (i-1, j)

                // Vertex (i, j-1) can be reached if there is no North-South pipe between tiles (i-1, j-1) and (i, j-1).
                if j > 0 && not (connectedByMainLoop ((tileI'J', South), (tileIJ', North))) then (i, j-1)

                // Vertex (i, j+1) can be reached if there is no North-South pipe between tiles (i-1, j) and (i, j).
                if j < columnCount && not (connectedByMainLoop ((tileI'J, South), (tileIJ, North))) then (i, j+1)

                // Vertex (i+1, j) can be reached if there is no East-West pipe between tiles (i, j-1) and (i, j).
                if i < rowCount && not (connectedByMainLoop ((tileIJ', East), (tileIJ, West))) then (i+1, j)
            }

            let edgeVertices =
                Seq.allPairs (seq { 0 .. rowCount + 1 }) (seq { 0 .. columnCount + 1 }) |> Set
                |> Set.filter (fun (i, j) -> i = 0 || i = rowCount + 1 || j = 0 || j = columnCount + 1)

            let unenclosedVertices =
                // Start with the vertices at the edge of grid.
                ((Set.empty, edgeVertices), Seq.initInfinite ignore)
                // Iteratively add all adjacent vertices that can be reached without crossing the main loop
                ||> Seq.scan
                    (fun (oldVertices, newVertices) _ ->
                        let verticesSoFar = Set.union oldVertices newVertices
                        let nextVertices =
                            newVertices
                            |> Seq.collect accessibleAdjacentVertices
                            |> Set
                            |> fun s -> Set.difference s verticesSoFar
                        verticesSoFar, nextVertices)
                // Until no new vertices are added.
                |> Seq.takeWhile (snd >> Set.isEmpty >> not)
                |> Seq.last
                ||> Set.union

            let enclosedTileCoords =
                let allTileCoords = Seq.allPairs (seq { 0 .. rowCount }) (seq { 0 .. columnCount }) |> Set

                unenclosedVertices
                |> Seq.map (fun (i, j) -> Set (seq { (i-1, j); (i-1, j-1); (i, j-1); (i, j) }))
                |> Set.unionMany
                |> fun unenclosedTileCoords -> Set.difference allTileCoords unenclosedTileCoords

            enclosedTileCoords |> Set.count

module Day11 =

    type Pixel =
        | EmptySpace
        | Galaxy

    module Pixel =

        let parse = function '.' -> EmptySpace | '#' -> Galaxy | c -> failwith $"Invalid pixel: %c{c}"

    let solve expansionFactor image =

        let universe = image |> Array2D.map Pixel.parse

        let rowNumbers = [| 0 .. (universe |> Array2D.length1) - 1 |]
        let colNumbers = [| 0 .. (universe |> Array2D.length2) - 1 |]

        let galaxyIndices =
            Seq.allPairs rowNumbers colNumbers
            |> Seq.filter (fun (i, j) -> Array2D.get universe i j = Galaxy)
            |> Seq.toArray

        let galaxyPairs =
            galaxyIndices
            |> Seq.mapi (fun i g1 -> galaxyIndices.[ (i+1) .. ] |> Array.map (fun g2 -> g1, g2))
            |> Seq.collect id

        let emptyRowNumbers =
            rowNumbers
            |> Array.filter (fun i -> colNumbers |> Array.forall (fun j -> Array2D.get universe i j = EmptySpace))
            |> Set

        let emptyColNumbers =
            colNumbers
            |> Array.filter (fun j -> rowNumbers |> Array.forall (fun i -> Array2D.get universe i j = EmptySpace))
            |> Set

        let shortestPathLength ((i1: int, j1: int), (i2, j2)) =

            let crossedEmptyRows =
                (if i2 > i1 then Set (seq { i1 .. i2 }) else Set (seq { i2 .. i1 }))
                |> Set.intersect emptyRowNumbers
                |> Set.count
                |> int64

            let crossedEmptyCols =
                (if j2 > j1 then Set (seq { j1 .. j2 }) else Set (seq { j2 .. j1 }))
                |> Set.intersect emptyColNumbers
                |> Set.count
                |> int64

            (Math.Abs(i2 - i1) |> int64)
            + (Math.Abs (j2 - j1) |> int64)
            + (expansionFactor - 1L) * (crossedEmptyRows + crossedEmptyCols)

        galaxyPairs |> Seq.sumBy shortestPathLength

    module PartOne =
        let solve = solve 2


    module PartTwo =
        let solve = solve 1_000_000L

module Day12 =

    type SpringStatus =
        | Operational
        | Damaged
        | Unknown

    module SpringStatus =

        let parse =
            function '.' -> Operational | '#' -> Damaged | '?' -> Unknown | c -> failwith $"Invalid status char: %c{c}"

    type RowInfo = {
        Row: SpringStatus array
        DamagedGroupSizes: int array
    }

    module RowInfo =

        let parse (line: string) =
            let parts = line.Split(' ')
            {
                Row = parts.[0] |> Seq.map SpringStatus.parse |> Seq.toArray
                DamagedGroupSizes = parts.[1].Split(',') |> Array.map int
            }

        let possibleArrangements info =

            let matchesPattern arrangement =
                arrangement
                |> Seq.zip info.Row
                |> Seq.forall (fun (r, a) -> r = a || r = Unknown)

            (info.DamagedGroupSizes, [ [] ])
            ||> Array.foldBack (fun groupSize possibleGroupStartIndices ->
                possibleGroupStartIndices
                |> List.collect (fun groupStartIndices ->

                    let remainingPattern =
                        match groupStartIndices with
                        | [] -> info.Row
                        // Go up to the first index and leave a space for an operational spring.
                        | index :: _ -> info.Row.[ 0 .. index - 2 ]

                    let possibleStartIndices =
                        remainingPattern
                        |> Array.windowed groupSize
                        |> Array.indexed
                        |> Array.filter (fun (_, window) ->
                            window |> Array.forall (function Operational -> false | Damaged | Unknown -> true))
                        |> Array.map fst
                        |> Array.toList

                    possibleStartIndices
                    |> List.map (fun i -> i :: groupStartIndices)))

            |> Seq.map (fun indices ->
                let damagedIndices =
                    info.DamagedGroupSizes
                    |> Seq.zip indices
                    |> Seq.collect (fun (startIndex, size) -> seq { startIndex .. startIndex + size - 1})
                    |> Seq.toArray

                seq {
                    for i in 0 .. info.Row.Length do
                        if damagedIndices |> Array.contains i then
                            Damaged
                        else
                            Operational
                })

            |> Seq.filter matchesPattern

    module PartOne =

        let solve lines =
            lines
            |> Array.map (RowInfo.parse >> RowInfo.possibleArrangements >> Seq.length)
            |> Array.sum

// FSI process has to run in same directory as this .fsx file for the relative path to work correctly.
"./day12input"
|> System.IO.File.ReadAllLines
|> Day12.PartOne.solve
