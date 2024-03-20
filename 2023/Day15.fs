module Day15

open System

let steps initializationSequence = initializationSequence |> String.filter ((<>) '\n') |> fun s -> s.Split(',')

let hash (input: char seq) = (0, input) ||> Seq.fold (fun currentValue c -> ((currentValue + (int c)) * 17 % 256))

module PartOne =

    let solve initializationSequence = initializationSequence |> steps |> Array.map hash |> Array.sum

module PartTwo =

    let solve initializationSequence =

        let boxes = Collections.Generic.Dictionary()
        for i in 0 .. 255 do
            boxes[i] <- [||]

        initializationSequence
        |> steps
        |> Seq.iter (fun step ->

            let span = step.AsSpan()

            let label = step |> Seq.take (span.LastIndexOfAny('-', '=')) |> Seq.toArray |> String
            let boxNumber = label |> hash
            let operationIndex = span.LastIndexOfAny('-', '=')

            match span.Item operationIndex with
            | '=' ->

                let prevContents = boxes.Item boxNumber

                let newContents =

                    let focalLength = span.Item (operationIndex + 1) |> string |> Int32.Parse

                    prevContents
                    |> Array.tryFindIndex (fst >> ((=) label))
                    |> Option.map (fun i -> prevContents |> Array.updateAt i (label, focalLength))
                    |> Option.defaultWith (fun _ -> Array.append prevContents [| (label, focalLength) |])

                boxes[boxNumber] <- newContents

            | '-' ->
                boxes.Item boxNumber
                |> Array.filter (fst >> ((<>) label))
                |> fun newContents -> boxes[boxNumber] <- newContents
            | c ->
                failwith $"Invalid operation character %c{c}")

        (0, seq { 0 .. 255 })
        ||> Seq.fold (fun sum boxNumber ->
            let boxSum =
                ((1, 0), boxes.[boxNumber])
                ||> Array.fold (fun (slotNumber, boxSum) (_label, focalLength) ->
                    slotNumber + 1, boxSum + (boxNumber + 1) * slotNumber * focalLength)
                |> snd
            sum + boxSum)

