module Day24

[<AutoOpen>]
module Utils =

    module Array =

        let distinctPairs xs =
            let indexed = Array.indexed xs
            indexed
            |> Array.collect (fun (i, x) -> indexed |> Array.skip (i + 1) |> Array.map (fun (_, x') -> (x, x')))

type Trajectory = {
    InitialX: float
    InitialY: float
    InitialZ: float
    VelocityX: float
    VelocityY: float
    VelocityZ: float
}

let parse lines =

    let parseLine (line: string) =

        let parts = line.Split(" @ ")
        let initials = parts[0].Split(", ")
        let velocities = parts[1].Split(", ")

        {
            InitialX = initials.[0] |> float
            InitialY = initials.[1] |> float
            InitialZ = initials.[2] |> float
            VelocityX = velocities.[0] |> float
            VelocityY = velocities.[1] |> float
            VelocityZ = velocities.[2] |> float
        }

    lines
    |> Array.map parseLine

module PartOne =

    type SinglePointIntersectionData = {
        X: float
        Y: float
        T1: float
        T2: float
    }

    type Intersection =
        | NoIntersection
        | SinglePoint of SinglePointIntersectionData

    let gradient { VelocityX = vx; VelocityY = vy } =
        if vx = 0.0 then failwith "Constant x is not handled."
        vy / vx

    let findIntersection ({InitialX = x1; InitialY = y1 } as tr1, ({InitialX = x2; InitialY = y2 } as tr2)) =

        let g1 = gradient tr1
        let g2 = gradient tr2

        if g1 <> g2 then
            let deltaX1 = (y2 - y1 + g2 * (x1 - x2)) / (g1 - g2)
            let deltaY1 = g1 * deltaX1
            let x = x1 + deltaX1
            let y = y1 + deltaY1
            let t1 = (x - x1) / tr1.VelocityX
            let t2 = (x - x2) / tr2.VelocityX
            SinglePoint { X = x; Y = y; T1 = t1; T2 = t2 }

        elif y2 - y1 = g1 * (x2 - x1) then
            failwith "whole line intersection not handled"

        else
            NoIntersection

    let solve atLeast atMost lines =
        lines
        |> parse
        |> Array.distinctPairs
        |> Array.map findIntersection
        |> Array.filter
            (function
            | NoIntersection ->
                false
            | SinglePoint { X = x; Y = y; T1 = t1; T2 = t2 } ->
                atLeast <= x && x <= atMost && atLeast <= y && y <= atMost && t1 >= 0 && t2 >= 0)
        |> Array.length

module Test =
    open Swensen.Unquote
    open Expecto

    let utilsTests = testList "Utils" [

        test "distinct pairs returns expected values" {
            let actual = Array.distinctPairs [| 1; 2; 3; 4; 5 |]
            Expect.equal actual [| 1, 2; 1, 3; 1, 4; 1, 5; 2, 3; 2, 4; 2, 5; 3, 4; 3, 5; 4, 5 |] ""
        }

    ]

    let sampleTests = testList "sample" [

        let sampleInput = [|
            "19, 13, 30 @ -2,  1, -2"
            "18, 19, 22 @ -1, -1, -2"
            "20, 25, 34 @ -2, -2, -4"
            "12, 31, 28 @ -1, -2, -1"
            "20, 19, 15 @  1, -5, -3"
        |]

        let sampleTrajectories = [|
            { InitialX = 19; InitialY = 13; InitialZ = 30; VelocityX = -2; VelocityY =  1; VelocityZ = -2 }
            { InitialX = 18; InitialY = 19; InitialZ = 22; VelocityX = -1; VelocityY = -1; VelocityZ = -2 }
            { InitialX = 20; InitialY = 25; InitialZ = 34; VelocityX = -2; VelocityY = -2; VelocityZ = -4 }
            { InitialX = 12; InitialY = 31; InitialZ = 28; VelocityX = -1; VelocityY = -2; VelocityZ = -1 }
            { InitialX = 20; InitialY = 19; InitialZ = 15; VelocityX =  1; VelocityY = -5; VelocityZ = -3 }
        |]

        test "parseLines returns correct data" {
            let actual = parse sampleInput
            Expect.equal actual sampleTrajectories ""
        }

        test "PartOne.gradient returns correct values" {
            let actual = sampleTrajectories |> Array.map PartOne.gradient
            Expect.equal actual [| -0.5; 1.0; 1.0; 2.0; -5.0 |] ""
        }

        test "PartOne.findIntersection returns correct (x, y) values for future intersections" {

            let actual =
                sampleTrajectories
                |> Array.distinctPairs
                |> Array.choose
                    (PartOne.findIntersection
                    >> function
                    | PartOne.NoIntersection ->
                        None
                    | PartOne.SinglePoint { X = x; Y = y; T1 = t1; T2 = t2 } ->
                        if t1 >= 0 && t2 >= 0 then Some (x, y) else None)

            let expected = [|
                let oneThird = 1.0 / 3.0
                let twoThirds = 2.0 / 3.0

                14.0 + oneThird, 15.0 + oneThird
                11.0 + twoThirds, 16.0 + twoThirds
                6.2, 19.4
                -6.0, -5.0
                -2.0, 3.0
            |]
            let withinTolerance (x1, y1) (x2, y2) =
                abs (x1 - x2) < 0.001
                && abs (y1 - y2) < 0.001

            (actual, expected)
            ||> Array.iter2 (fun a e -> Assertions.test <@ withinTolerance a e @>)

        }

        test "PartOne.solve gives correct answer" {
            let actual = PartOne.solve 7 27 sampleInput
            Expect.equal actual 2 ""
        }

    ]

    let all = testList "Day 24" [
        utilsTests
        sampleTests
    ]
