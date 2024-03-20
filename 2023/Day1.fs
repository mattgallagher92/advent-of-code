module Day1

module PartOne =

    open System

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
