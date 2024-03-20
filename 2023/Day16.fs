module Day16

type Tile =
    | EmptySpace
    | FMirror
    | BMirror
    | HSplitter
    | VSplitter

type Direction =
    | Upward
    | Downward
    | Leftward
    | Rightward

type BeamSegment = {
    Row: int
    Col: int
    Direction: Direction
}

let parse lines =
    lines
    |> array2D
    |> Array2D.map (
        function
        | '.' -> EmptySpace
        | '/' -> FMirror
        | '\\' -> BMirror
        | '-' -> HSplitter
        | '|' -> VSplitter
        | c -> failwith $"Invalid tile char '%c{c}'")

/// Returns a function that, given a segment, returns the segments that come next.
let nextSegments layout =

    let height, width = layout |> Array2D.length1, layout |> Array2D.length2

    // Return anonymous function to avoid recalculation of height and width.
    fun segment ->

        let up =
            if segment.Row > 0 then
                Some { segment with Row = segment.Row - 1; Direction = Upward }
            else
                None
        let down =
            if segment.Row < height - 1 then
                Some { segment with Row = segment.Row + 1; Direction = Downward }
            else
                None
        let left =
            if segment.Col > 0 then
                Some { segment with Col = segment.Col - 1; Direction = Leftward }
            else
                None
        let right =
            if segment.Col < width - 1 then
                Some { segment with Col = segment.Col + 1; Direction = Rightward }
            else
                None

        match layout.[ segment.Row, segment.Col ], segment.Direction with
        | (EmptySpace | VSplitter), Upward -> [| up |]
        | (EmptySpace | VSplitter), Downward -> [| down |]
        | (EmptySpace | HSplitter), Leftward -> [| left |]
        | (EmptySpace | HSplitter), Rightward -> [| right |]
        | VSplitter, (Leftward | Rightward) -> [| up; down |]
        | HSplitter, (Upward | Downward) -> [| left; right |]
        | FMirror, Upward -> [| right |]
        | FMirror, Downward -> [| left |]
        | FMirror, Leftward -> [| down |]
        | FMirror, Rightward -> [| up |]
        | BMirror, Upward -> [| left |]
        | BMirror, Downward -> [| right |]
        | BMirror, Leftward -> [| up |]
        | BMirror, Rightward -> [| down |]
        |> Array.choose id

/// A sequence whose nth element is an array of BeamSegments that the beam is in n steps after the startSegment.
let steps layout startSegment =
    [| startSegment |]
    |> Seq.unfold (Array.collect (nextSegments layout) >> function [||] -> None | next -> Some (next, next))
    |> Seq.append (seq { [| startSegment |] })

/// Returns a function that, given a BeamSegment, returns the path starting from it until it splits or leaves the
/// grid. The path before the split or exit is returned as the first element of the pair; the second element
/// contains the segments that come after the split or exit.
let pathUntilSplitOrExit layout =

    fun startSegment ->

        startSegment
        |> steps layout
        |> Seq.takeUntilFirstMatch (Array.length >> ((<>) 1))
        |> Seq.toArray
        |> fun until ->
            let beforeSplitOrExit = until |> Array.take (until.Length - 1) |> Array.map Array.head
            let afterSplitOrExit = until |> Array.last
            beforeSplitOrExit, afterSplitOrExit

    |> memoize

let countEnergizedTiles layout startSegment =

    let pathUntilSplitOrExit = pathUntilSplitOrExit layout

    let startSegments =
        (Set.empty, Set [ startSegment ])
        |> Seq.unfold
            (fun (oldStartSegments, startSegments) ->

                let oldSegments = Set.union oldStartSegments startSegments

                let newStartSegments =
                    startSegments
                    |> Set.map (pathUntilSplitOrExit >> snd >> set)
                    |> Set.unionMany
                    |> fun s -> Set.difference s oldSegments

                if newStartSegments |> Set.isEmpty then
                    None
                else
                    Some ((oldSegments |> Set.union newStartSegments, (oldSegments, newStartSegments))))
        |> Seq.last

    startSegments
    |> Set.map (fun x -> x |> pathUntilSplitOrExit |> fst |> Array.append [| x |] |> set)
    |> Set.unionMany
    |> Set.map (fun seg -> seg.Row, seg.Col)
    |> Set.count

module PartOne =

    let solve lines = countEnergizedTiles (parse lines) { Row = 0; Col = 0; Direction = Rightward }

module PartTwo =

    // TODO: speed up - takes about 15 seconds.
    let solve lines =

        let layout = parse lines

        seq {

            let height, width = layout |> Array2D.length1, layout |> Array2D.length2

            for i in 0 .. height - 1 do
                { Row = i; Col = 0; Direction = Rightward }
                { Row = i; Col = width - 1; Direction = Leftward }

            for j in 0 .. width - 1 do
                { Row = 0; Col = j; Direction = Downward }
                { Row = height - 1; Col = j; Direction = Upward }

        }
        |> Seq.map (countEnergizedTiles layout)
        |> Seq.max

