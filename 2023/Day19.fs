module Day19

open System

type Condition =
    | LessThan of int
    | GreaterThan of int

type ConditionalRule = {
    Category: char
    Condition: Condition
    Destination: string
}

type Rules =
    | Unconditional of string
    | Conditional of ConditionalRule * Rules

type Workflow = {
    Name: string
    Rules: Rules
}

let parseWorkflow (line: string) =

    let openBraceIx = line |> Seq.findIndex ((=) '{')

    {
        Name = line.Substring(0, openBraceIx)
        Rules =
            let strings =
                line
                    .Substring(openBraceIx + 1, line.Length - openBraceIx - 2)
                    .Split(',')
            (strings.[ .. strings.Length - 2], strings |> Array.last |> Unconditional)
            ||> Array.foldBack (fun s rule->
                    let colonIx = s |> Seq.findIndex ((=) ':')
                    let conditionType =
                        match s.Chars 1 with
                        | '<' -> LessThan
                        | '>' -> GreaterThan
                        | _ -> failwith "Invalid rule"
                    {
                        Category = s.Chars 0
                        Condition = s.Substring(2, colonIx - 2) |> int |> conditionType
                        Destination = s.Substring(colonIx + 1, s.Length - colonIx - 1)
                    }
                    |> fun conditional -> Conditional (conditional, rule))
    }

type Rating = {
    X: int
    M: int
    A: int
    S: int
}

let parseRating (line: string) =
    line.Substring(1, line.Length - 2)
    |> (fun s ->
        let parts = s.Split(',')
        let x, m, a, s = parts.[0], parts.[1], parts.[2], parts.[3]
        {
            X = x.Substring(2, x.Length - 2) |> int
            M = m.Substring(2, m.Length - 2) |> int
            A = a.Substring(2, a.Length - 2) |> int
            S = s.Substring(2, s.Length - 2) |> int
        })

let parse (text: string) =
    let tokens = text.Split("\n\n")
    let workflows = tokens.[0].Split('\n') |> Array.map parseWorkflow
    let ratings = tokens.[1].Split('\n') |> Array.filter (not << String.IsNullOrWhiteSpace) |> Array.map parseRating

    workflows, ratings
module PartOne =

    let rec applyRule ({ X = x; M = m; A = a; S = s } as rating) =
        function
        | Unconditional d ->
            d
        | Conditional ({ Category = 'x'; Condition = LessThan i; Destination = d }, next) ->
            if x < i then d else applyRule rating next
        | Conditional ({ Category = 'x'; Condition = GreaterThan i; Destination = d }, next) ->
            if x > i then d else applyRule rating next
        | Conditional ({ Category = 'm'; Condition = LessThan i; Destination = d }, next) ->
            if m < i then d else applyRule rating next
        | Conditional ({ Category = 'm'; Condition = GreaterThan i; Destination = d }, next) ->
            if m > i then d else applyRule rating next
        | Conditional ({ Category = 'a'; Condition = LessThan i; Destination = d }, next) ->
            if a < i then d else applyRule rating next
        | Conditional ({ Category = 'a'; Condition = GreaterThan i; Destination = d }, next) ->
            if a > i then d else applyRule rating next
        | Conditional ({ Category = 's'; Condition = LessThan i; Destination = d }, next) ->
            if s < i then d else applyRule rating next
        | Conditional ({ Category = 's'; Condition = GreaterThan i; Destination = d }, next) ->
            if s > i then d else applyRule rating next
        | Conditional ({ Category = c }, _) ->
            failwith $"Invalid rating category: %c{c}"

    let ratingSum { X = x; M = m; A = a; S = s } = x + m + a + s

    let isAccepted workflows =

        let rulesLookup = workflows |> Array.map (fun w -> w.Name, w.Rules) |> Map |> Collections.Generic.Dictionary

        let rec inner workflowName rating =

            rulesLookup.Item workflowName
            |> applyRule rating
            |> function
                | "A" -> true
                | "R" -> false
                | d -> inner d rating

        inner "in"

    let solve text =

        let workflows, ratings = parse text

        ratings
        |> Array.filter (isAccepted workflows)
        |> Array.sumBy ratingSum

module PartTwo =

    type RatingRange = {
        XMin: int
        XMax: int
        MMin: int
        MMax: int
        AMin: int
        AMax: int
        SMin: int
        SMax: int
    }

    module RatingRange =

        let min category range =
            match category with
            | 'x' -> range.XMin
            | 'm' -> range.MMin
            | 'a' -> range.AMin
            | 's' -> range.SMin
            | c -> failwith $"Invalid category %c{c}"

        let max category range =
            match category with
            | 'x' -> range.XMax
            | 'm' -> range.MMax
            | 'a' -> range.AMax
            | 's' -> range.SMax
            | c -> failwith $"Invalid category %c{c}"

        let updateMin category newMin range =
            match category with
            | 'x' -> { range with XMin = newMin }
            | 'm' -> { range with MMin = newMin }
            | 'a' -> { range with AMin = newMin }
            | 's' -> { range with SMin = newMin }
            | c -> failwith $"Invalid category %c{c}"

        let updateMax category newMax range =
            match category with
            | 'x' -> { range with XMax = newMax }
            | 'm' -> { range with MMax = newMax }
            | 'a' -> { range with AMax = newMax }
            | 's' -> { range with SMax = newMax }
            | c -> failwith $"Invalid category %c{c}"

        let rec split rules range =

            match rules with
            | Unconditional d ->
                seq { range, d }

            | Conditional ({ Category = cat; Condition = LessThan i; Destination = d }, next) ->

                let matching =
                    let oldMax = max cat range
                    let newMax = Math.Min(oldMax, i - 1)
                    range |> updateMax cat newMax

                let nonMatching =
                    let oldMin = min cat range
                    let newMin = Math.Max(oldMin, i)
                    range |> updateMin cat newMin

                seq { matching, d; yield! split next nonMatching }

            | Conditional ({ Category = cat; Condition = GreaterThan i; Destination = d }, next) ->

                let matching =
                    let oldMin = min cat range
                    let newMin = Math.Max(oldMin, i + 1)
                    range |> updateMin cat newMin

                let nonMatching =
                    let oldMax = max cat range
                    let newMax = Math.Min(oldMax, i)
                    range |> updateMax cat newMax

                seq { matching, d; yield! split next nonMatching }

        let count range =
            ((range.XMax - range.XMin + 1) |> int64)
            * ((range.MMax - range.MMin + 1) |> int64)
            * ((range.AMax - range.AMin + 1) |> int64)
            * ((range.SMax - range.SMin + 1) |> int64)

    let acceptedRanges workflows =

        let rulesLookup = workflows |> Array.map (fun w -> w.Name, w.Rules) |> Map |> Collections.Generic.Dictionary

        let rec inner (range: RatingRange) =
            function
            | "A" -> seq { range }
            | "R" -> Seq.empty
            | workflowName ->
                let rules = rulesLookup.Item workflowName
                range |> RatingRange.split rules
                |> Seq.collect (fun (r, n) -> inner r n)

        ({ XMin = 1; XMax = 4000; MMin = 1; MMax = 4000; AMin = 1; AMax = 4000; SMin = 1; SMax = 4000 }, "in")
        ||> inner
        |> Seq.sumBy RatingRange.count

    let solve text = text |> parse |> fst |> acceptedRanges

