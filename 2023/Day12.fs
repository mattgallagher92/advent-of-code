module Day12

open System.Collections.Generic

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

    // Need to use the structural equality comparer to ensure that different arrays with the same contents are
    // considered to be the same key.
    let private cache = Dictionary<SpringStatus array * int array, int64>(HashIdentity.Structural)

    let countPossibleArrangements info =

        let groupDoesNotFitAtStartOf (statuses: SpringStatus array) groupSize =

            let groupIsTooBig = groupSize > statuses.Length
            let groupWouldIntersectOperationalSpring = statuses.[ .. groupSize - 1 ] |> Array.contains Operational
            let followingSpringIsDamaged =
                statuses
                |> Array.tryItem groupSize
                |> Option.map ((=) Damaged)
                |> Option.defaultValue false

            groupIsTooBig
            || groupWouldIntersectOperationalSpring
            || followingSpringIsDamaged

        let rec inner =
            function
            | [||], [||] ->
                1L
            | [||], _ ->
                0L
            | statuses, [||] ->
                if statuses |> Array.contains Damaged then 0L else 1L
            | statuses, groupSizes ->

                cache.TryGet (statuses, groupSizes)
                |> Option.defaultWith (fun _ ->

                    let numberOfPossibleArrangements =

                        let countOperational () = inner (Array.tail statuses, groupSizes)

                        let countDamaged () =
                            let groupSize = Array.head groupSizes
                            if groupSize |> groupDoesNotFitAtStartOf statuses then
                                0L
                            else
                                inner (statuses.[ groupSize + 1 .. ], Array.tail groupSizes)

                        match Array.head statuses with
                        | Operational -> countOperational ()
                        | Damaged -> countDamaged ()
                        | Unknown -> countOperational () + countDamaged ()

                    cache.Add((statuses, groupSizes), numberOfPossibleArrangements)

                    numberOfPossibleArrangements)

        inner (info.Row, info.DamagedGroupSizes)

let solve parse lines =
    lines
    |> Array.map (parse >> RowInfo.countPossibleArrangements)
    |> Array.sum

module PartOne =

    let parseRowInfo (line: string) =
        let parts = line.Split(' ')
        {
            Row = parts.[0] |> Seq.map SpringStatus.parse |> Seq.toArray
            DamagedGroupSizes = parts.[1].Split(',') |> Array.map int
        }

    let solve = solve parseRowInfo

module PartTwo =

    let parseRowInfo line =
        line
        |> PartOne.parseRowInfo
        |> fun { Row = row; DamagedGroupSizes = sizes } ->
            {
                Row =
                    [| row; [| Unknown |]; row; [| Unknown |]; row; [| Unknown |]; row; [| Unknown |]; row |]
                    |> Array.collect id
                DamagedGroupSizes =
                    sizes
                    |> Array.replicate 5
                    |> Array.collect id
            }

    let solve = solve parseRowInfo

