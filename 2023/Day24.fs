module Day24

open MathNet.Numerics

[<AutoOpen>]
module Utils =

    module Array =

        let distinctPairs xs =
            let indexed = Array.indexed xs
            indexed
            |> Array.collect (fun (i, x) -> indexed |> Array.skip (i + 1) |> Array.map (fun (_, x') -> (x, x')))

type Trajectory = {
    InitialX: int64
    InitialY: int64
    InitialZ: int64
    VelocityX: int64
    VelocityY: int64
    VelocityZ: int64
}

let parse lines =

    let parseLine (line: string) =

        let parts = line.Split(" @ ")
        let initials = parts[0].Split(", ")
        let velocities = parts[1].Split(", ")

        {
            InitialX = initials.[0] |> int64
            InitialY = initials.[1] |> int64
            InitialZ = initials.[2] |> int64
            VelocityX = velocities.[0] |> int64
            VelocityY = velocities.[1] |> int64
            VelocityZ = velocities.[2] |> int64
        }

    lines
    |> Array.map parseLine

module PartOne =

    type SinglePointIntersectionData = {
        X: BigRational
        Y: BigRational
        T1: BigRational
        T2: BigRational
    }

    type Intersection =
        | NoIntersection
        | SinglePoint of SinglePointIntersectionData

    let gradient { VelocityX = vx; VelocityY = vy } =
        if vx = 0 then None else Some (BigRational.FromBigIntFraction(vy, vx))

    let findIntersection (tr1, tr2) =

        let x1 = BigRational.FromBigInt tr1.InitialX
        let y1 = BigRational.FromBigInt tr1.InitialY
        let vx1 = BigRational.FromBigInt tr1.VelocityX
        let x2 = BigRational.FromBigInt tr2.InitialX
        let y2 = BigRational.FromBigInt tr2.InitialY
        let vx2 = BigRational.FromBigInt tr2.VelocityX

        match gradient tr1, gradient tr2 with
        | Some g1, Some g2 when g1 <> g2 ->
            let deltaX1 = (y2 - y1 + g2 * (x1 - x2)) / (g1 - g2)
            let deltaY1 = g1 * deltaX1
            let x = x1 + deltaX1
            let y = y1 + deltaY1
            let t1 = (x - x1) / vx1
            let t2 = (x - x2) / vx2
            SinglePoint { X = x; Y = y; T1 = t1; T2 = t2 }

        | Some g1, Some _ when y2 - y1 = g1 * (x2 - x1) ->
            failwith "whole line intersection not handled"

        | Some _, Some _ ->
            NoIntersection

        | Some g1, None ->
            let x = x2
            let y = y1 + g1 * (x2 - x1)
            let t1 = (x - x1) / vx1
            SinglePoint { X = x; Y = y; T1 = t1; T2 = t1 }

        | None, Some g2 ->
            let x = x1
            let y = y2 + g2 * (x1 - x2)
            let t2 = (x - x2) / vx2
            SinglePoint { X = x; Y = y; T1 = t2; T2 = t2 }

        | None, None when x1 = x2 ->
            failwith "whole line intersection not handled"

        | None, None ->
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
                atLeast <= x && x <= atMost
                && atLeast <= y && y <= atMost
                && t1 >= 0N
                && t2 >= 0N)
        |> Array.length

module PartTwo =

    let relativeToVelocity (vx, vy, vz) tr =
        { tr with VelocityX = tr.VelocityX - vx; VelocityY = tr.VelocityY - vy; VelocityZ = tr.VelocityZ - vz }

    let findIntersectionPoint tr1 tr2 : (BigRational * BigRational * BigRational) option =

        // Find x-y intersection.
        match PartOne.findIntersection(tr1, tr2) with
        // Check that times are both non-negative.
        | PartOne.SinglePoint { X = x; Y = y; T1 = t1; T2 = t2 } when t1 >= 0N && t2 >= 0N ->
            // Calculate z coordinate, checking it's the same for both trajectories.
            let z1 = BigRational.FromBigInt tr1.InitialZ + t1 * BigRational.FromBigInt tr1.VelocityZ
            let z2 = BigRational.FromBigInt tr2.InitialZ + t2 * BigRational.FromBigInt tr2.VelocityZ
            if z1 = z2 then
                Some (x, y, z1)
            else
                None

        | _ ->
            None

    type CoordinateIntersectionTime =
        | Always
        | SingleTime of BigRational
        | Never

    /// Checks whether the given trajectory passes through the given point (at a non-negative time).
    let intersectsPoint (x, y, z) tr =

        let dX = x - (tr.InitialX |> BigRational.FromBigInt)
        let dY = y - (tr.InitialY |> BigRational.FromBigInt)
        let dZ = z - (tr.InitialZ |> BigRational.FromBigInt)

        let vX = tr.VelocityX |> BigRational.FromBigInt
        let vY = tr.VelocityY |> BigRational.FromBigInt
        let vZ = tr.VelocityZ |> BigRational.FromBigInt

        let intersectionTime v d =
            if v = 0N && d = 0N then
                Always
            elif v = 0N then
                Never
            else
                SingleTime (d / v)

        match intersectionTime vX dX, intersectionTime vY dY, intersectionTime vZ dZ with
        | Always, Always, Always -> true
        | SingleTime t, Always, Always
        | Always, SingleTime t, Always
        | Always, Always, SingleTime t -> t >= 0N
        | SingleTime t1, SingleTime t2, Always
        | SingleTime t1, Always, SingleTime t2
        | Always, SingleTime t1, SingleTime t2 -> t1 >= 0N && t1 = t2
        | SingleTime tX, SingleTime tY, SingleTime tZ -> tX >= 0N && tX = tY && tY = tZ
        | Never, _, _
        | _, Never, _
        | _, _, Never -> false

    /// Must be called with at least two trajectories.
    let commonIntersectionPoint (trs: Trajectory array) : (BigRational * BigRational * BigRational) option =
        findIntersectionPoint trs.[0] trs.[1]
        |> Option.bind (fun candidate ->
            if trs.[ 2 .. ] |> Array.forall (intersectsPoint candidate) then
                Some candidate
            else
                None)

    type InclusiveRange = {
        Start: int64
        End: int64
    }

    type VelocityRanges = {
        X: InclusiveRange array
        Y: InclusiveRange array
        Z: InclusiveRange array
    }

    /// Ranges of velocity coordinates for which any velocity with a coordinate in one of the ranges is not a viable
    /// candidate for the solution.
    let impossibleVelocityRanges trajectories : VelocityRanges =

        let impossibleRanges (positionProjection: Trajectory -> int64) (velocityProjection: Trajectory -> int64) =

            (([||], []), (trajectories |> Array.sortByDescending velocityProjection))
            ||> Array.fold (fun (maxima, impossibleRanges) tr ->

                let v = velocityProjection tr
                let p = positionProjection tr

                // Velocity-position pairs for which pairs with equal or higher velocity have equal or lower position.
                let newMaxima =
                    if maxima |> Array.isEmpty then
                        [| v, p |]
                    else
                        let u, highestP = maxima.[0]
                        if p > highestP then
                            // TODO: could avoid this check by better sorting of trajectories.
                            if u = v then[| (v, p); yield! maxima.[ 1 .. ] |] else [| (v, p); yield! maxima |]
                        else
                            maxima

                // If p1 < p2 and v1 <= v2 then any velocity v in the range v1 to v2 is impossible. This is because the
                // trajectory starting at p1 with velocity v1 - v and the trajectory starting at p2 with velocity v2 - v
                // don't intersect (because they don't move toward the other p).
                // Consideration can be limited to the maxima because if a pair is not a maximum then there is a point
                // with equal or higher velocity and higher position; the ranges given by non-maxima are therefore
                // contained in those given by maxima.
                let newImpossibleRanges =

                    match maxima |> Array.tryFindBack (fun (_, q) -> q > p) with
                    | Some (higherV, _) ->
                        let intersectedRanges, others =
                            impossibleRanges |> List.partition (fun { Start = s } -> s <= higherV)
                        let newImpossibleRange =
                            ({ Start = v; End = higherV }, intersectedRanges)
                            ||> List.fold (fun { End = e1 } { End = e2 } -> { Start = v; End = max e1 e2 })
                        newImpossibleRange :: others

                    | None ->
                        impossibleRanges

                newMaxima, newImpossibleRanges)
            |> snd
            |> List.toArray

        {
            X = impossibleRanges _.InitialX _.VelocityX
            Y = impossibleRanges _.InitialY _.VelocityY
            Z = impossibleRanges _.InitialZ _.VelocityZ
        }

    let possibleVelocityComponents maxVelocityComponent trajectories =

        let potentialComponentValues = set [| -maxVelocityComponent .. maxVelocityComponent |]

        let impossibleRanges = impossibleVelocityRanges trajectories
        let toImpossibleSet ranges =
            (Set.empty, ranges)
            ||> Array.fold (fun set r -> (set, seq { r.Start .. r.End }) ||> Seq.fold (fun s i -> s |> Set.add i))

        Set.difference potentialComponentValues (toImpossibleSet impossibleRanges.X),
        Set.difference potentialComponentValues (toImpossibleSet impossibleRanges.Y),
        Set.difference potentialComponentValues (toImpossibleSet impossibleRanges.Z)

    // Test each velocity for a common intersection point amongst the relative trajectories; once such a velocity is
    // found, trajectory is one with that velocity, with initial position at that intersection point.
    let firstTrajectoryWhereRelativeTrajectoriesAllIntersectOrigin maxVelocityComponent trajectories : Trajectory option =

        let possibleVxs, possibleVys, possibleVzs = possibleVelocityComponents maxVelocityComponent trajectories
        seq {
            for vx in possibleVxs do
                for vy in possibleVys do
                    for vz in possibleVzs do
                        vx, vy, vz
        }
        |> Seq.choose (fun (vX, vY, vZ as v) ->
            trajectories
            |> Array.map (relativeToVelocity v)
            |> commonIntersectionPoint
            |> Option.map (fun (x, y, z) ->
                {
                    InitialX = if x.IsInteger then int64 x.Numerator else failwith $"x is not an integer %A{x}"
                    InitialY = if y.IsInteger then int64 y.Numerator else failwith $"y is not an integer %A{y}"
                    InitialZ = if z.IsInteger then int64 z.Numerator else failwith $"z is not an integer %A{z}"
                    VelocityX = vX; VelocityY = vY; VelocityZ = vZ
                }))
        |> Seq.tryHead

    let solve lines =
        lines
        |> parse
        |> firstTrajectoryWhereRelativeTrajectoriesAllIntersectOrigin 500
        |> Option.map (fun tr -> tr.InitialX + tr.InitialY + tr.InitialZ)

module Test =
    open Swensen.Unquote
    open Expecto

    let all = testList "Day 24" [

        testList "Utils" [

            test "distinct pairs returns expected values" {
                let actual = Array.distinctPairs [| 1; 2; 3; 4; 5 |]
                Expect.equal actual [| 1, 2; 1, 3; 1, 4; 1, 5; 2, 3; 2, 4; 2, 5; 3, 4; 3, 5; 4, 5 |] ""
            }

        ]

        testList "sample" [

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

            test "parse returns correct data" {
                let actual = parse sampleInput
                Expect.equal actual sampleTrajectories ""
            }

            testList "PartOne" [

                test "gradient returns correct values" {
                    let actual = sampleTrajectories |> Array.map PartOne.gradient

                    let expected = [|
                        -1N / 2N |> Some
                        1N |> Some
                        1N |> Some
                        2N |> Some
                        -5N |> Some
                    |]
                    Assertions.test <@ (actual, expected) ||> Array.forall2 _.Equals @>
                }

                test "findIntersection returns correct (x, y) values for future intersections" {

                    let actual =
                        sampleTrajectories
                        |> Array.distinctPairs
                        |> Array.choose
                            (PartOne.findIntersection
                            >> function
                            | PartOne.NoIntersection ->
                                None
                            | PartOne.SinglePoint { X = x; Y = y; T1 = t1; T2 = t2 } ->
                                if t1 >= 0N && t2 >= 0N then Some (x, y) else None)

                    let expected = [|
                        43N / 3N, 46N / 3N
                        35N / 3N, 50N / 3N
                        31N / 5N, 97N / 5N
                        -6N, -5N
                        -2N, 3N
                    |]

                    Assertions.test
                        <@ (actual, expected) ||> Array.forall2 (fun (a, b) (c, d) -> a.Equals(c) && b.Equals(d)) @>
                }

                test "solve gives correct answer" {
                    let actual = PartOne.solve 7N 27N sampleInput
                    Expect.equal actual 2 ""
                }

            ]

            testList "PartTwo" [

                let expectedVelocity = -3L, 1L, 2L
                let expectedIntersectionPoint = 24N, 13N, 10N
                let relativeTrajectories = sampleTrajectories |> Array.map (PartTwo.relativeToVelocity expectedVelocity)

                test "relativeToVelocity gives correct trajectories" {
                    let trajectories = [|
                        { InitialX = 19; InitialY = 13; InitialZ = 30; VelocityX = -2; VelocityY =  1; VelocityZ = -2 }
                        { InitialX = 18; InitialY = 19; InitialZ = 22; VelocityX = -1; VelocityY = -1; VelocityZ = -2 }
                    |]
                    let actual = trajectories |> Array.map (PartTwo.relativeToVelocity expectedVelocity)

                    let expected = [|
                        { trajectories.[0] with VelocityX = 1; VelocityY =  0; VelocityZ = -4 }
                        { trajectories.[1] with VelocityX = 2; VelocityY = -2; VelocityZ = -4 }
                    |]
                    Assertions.test <@ actual = expected @>
                }

                test "findIntersectionPoint returns expected intersection point" {
                    let actual = PartTwo.findIntersectionPoint relativeTrajectories.[0] relativeTrajectories.[1]
                    Assertions.test <@ actual = Some expectedIntersectionPoint @>
                }

                test "intersectsPoint returns true for all trajectories and sample intersection point" {
                    Assertions.test
                        <@ relativeTrajectories |> Array.forall (PartTwo.intersectsPoint expectedIntersectionPoint) @>
                }

                test "commonIntersectionPoint gives correct point" {
                    let actual = relativeTrajectories |> PartTwo.commonIntersectionPoint

                    Assertions.test <@ actual = Some expectedIntersectionPoint @>
                }

                test "impossibleVelocityRanges returns expected data" {
                    let actual = PartTwo.impossibleVelocityRanges sampleTrajectories

                    let expected = {
                        PartTwo.X = [| { Start = -2; End = 1 } |]
                        PartTwo.Y = [| { Start = -5; End = -2 } |]
                        PartTwo.Z = [| { Start = -3; End = -1 } |]
                    }
                    Assertions.test <@ actual = expected @>
                }

                test "" {
                    let actual = PartTwo.possibleVelocityComponents 5 sampleTrajectories

                    let expected =
                        set [| -5L; -4; -3; 2; 3; 4; 5 |],
                        set [| -1L; 0; 1; 2; 3; 4; 5 |],
                        set [| -5L; -4; 0; 1; 2; 3; 4; 5 |]
                    Assertions.test <@ actual = expected @>
                }

                test "firstTrajectoryWhereRelativeTrajectoriesAllIntersectOrigin gives correct trajectory" {
                    let actual = PartTwo.firstTrajectoryWhereRelativeTrajectoriesAllIntersectOrigin 10 sampleTrajectories

                    let expected =
                        { InitialX = 24; InitialY = 13; InitialZ = 10; VelocityX = -3; VelocityY = 1; VelocityZ = 2 }
                    Assertions.test <@ actual = Some expected @>
                }

            ]

        ]

        testList "PartOne" [

            testList "findIntersection" [

                test "Works when vx1 = 0 and vx2 <> 0" {
                    let tr1 = { InitialX = -1; InitialY = 2; InitialZ = 3; VelocityX = 0; VelocityY = 4; VelocityZ = 5 }
                    let tr2 = { InitialX = 1; InitialY = 5; InitialZ = 2; VelocityX = -1; VelocityY = 9; VelocityZ = 8 }

                    let actual = PartOne.findIntersection(tr1, tr2)

                    Assertions.test <@ actual = PartOne.SinglePoint { X = -1N; Y = 23N; T1 = 2N; T2 = 2N }  @>
                }

                test "Works when vx1 <> 0 and vx2 = 0" {
                    let tr1 = { InitialX = 7; InitialY = 3; InitialZ = 3; VelocityX = -2; VelocityY = 1; VelocityZ = 8 }
                    let tr2 = { InitialX = 1; InitialY = 7; InitialZ = 0; VelocityX = 0; VelocityY = 4; VelocityZ = 6 }

                    let actual = PartOne.findIntersection(tr1, tr2)

                    Assertions.test <@ actual = PartOne.SinglePoint { X = 1N; Y = 6N; T1 = 3N; T2 = 3N }  @>
                }

            ]

        ]

        testList "PartTwo" [

            test "intersectsPoint returns false for different point when velocity is zero" {
                let actual =
                    { InitialX = 0; InitialY = 0; InitialZ = 0; VelocityX = 0; VelocityY = 0; VelocityZ = 0 }
                    |> PartTwo.intersectsPoint (1N, 1N, 1N)
                Assertions.test <@ actual = false @>
            }

        ]

    ]
