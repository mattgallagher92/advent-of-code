module Day14

/// The new platform arrangement after tilting towards the start of all of the lines (so if lines are the
/// columns, this tilts towards the top row).
let tilt lineLength lines =

    lines
    |> Seq.map (fun col ->

        // Produce a sequence of pairs of the indices (counted from the northernmost row) where the boulders
        // stop and the number of boulders lined up from that point.
        let pairs =
            ([ (0, 0) ], col |> Seq.indexed)
            ||> Seq.fold (fun list (i, c) ->
                match c, list with
                | '.', (a, n) :: tail -> (a, n) :: tail
                | 'O', (a, n) :: tail -> (a, n + 1) :: tail
                | '#', list -> (i + 1, 0) :: list
                | _, [] -> failwith "Bug: invalid state"
                | c, _ -> failwith $"Column %A{col} has invalid character '%c{c}' in position %i{i}")
            |> Seq.filter (snd >> ((<>) 0))

        let roundIndices =
            (Seq.empty, pairs |> Seq.rev)
            ||> Seq.fold (fun state (i, n) -> seq {
                yield! state
                yield! seq { i .. i + n - 1 }
            })
            |> Set

        let cubeIndices = col |> Seq.indexed |> Seq.filter (snd >> ((=) '#')) |> Seq.map fst |> Set

        seq {
            for i in 0 .. lineLength - 1 do
                if roundIndices |> Set.contains i then 'O'
                elif cubeIndices |> Set.contains i then '#'
                else '.'
        })

let tiltNorth height rows = rows |> Seq.transpose |> tilt height |> Seq.transpose
let tiltWest width rows = rows |> tilt width
let tiltSouth height rows = rows |> Seq.transpose |> Seq.map Seq.rev |> tilt height |> Seq.map Seq.rev |> Seq.transpose
let tiltEast width rows = rows |> Seq.map Seq.rev |> tilt width |> Seq.map Seq.rev

let totalLoad rows =
    rows
    |> Seq.transpose
    |> Seq.collect (Seq.rev >> Seq.mapi (fun i x -> ((i + 1), x))  >> Seq.filter (snd >> ((=) 'O')) >> Seq.map fst)
    |> Seq.sum

module PartOne =

    let solve lines =
        lines
        |> tiltNorth (Array.length lines)
        |> totalLoad

module PartTwo =

    let spin height width rows =

        rows
        |> tiltNorth height
        |> tiltWest width
        |> tiltSouth height
        |> tiltEast width
        // Evaluate the sequences to avoid multiple evaluations when checking equality of elements.
        |> Seq.map Seq.toArray
        |> Seq.toArray

    let solve lines =

        let rows = lines |> Array.map Seq.toArray

        let elementCalculator =
            // TODO: explain why this falls into a cycle before one billion elements. It feels right, but I don't
            // have proof that it would for every possible puzzle input.
            Seq.repeatingSequenceElementCalculator
                (spin (rows |> Array.length) (rows |> Array.head |> Array.length))
                rows

        elementCalculator 1_000_000_000
        |> totalLoad

