module Day6

open System

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

