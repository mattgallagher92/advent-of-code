open System

module Day1 =
    module PartOne =
        let solve () =
            let toCalibrationValue line =
                line
                |> String.filter Char.IsDigit
                |> fun cs -> Int32.Parse $"%c{Seq.head cs}%c{Seq.last cs}"

            // FSI process has to run in same directory as this .fsx file for the relative path to work correctly.
            "./day1input"
            |> System.IO.File.ReadLines
            |> Seq.sumBy toCalibrationValue

Day1.PartOne.solve ()