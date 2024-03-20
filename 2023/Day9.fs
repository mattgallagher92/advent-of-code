module Day9

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

