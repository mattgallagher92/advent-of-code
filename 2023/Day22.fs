module Day22

open System
open System.Collections.Generic

type Brick = {
    Start: int * int * int
    End: int * int * int
}

module Brick =

    let lowestZ { Start = _, _, zs; End = _, _, ze } = Math.Min(zs, ze)

    let highestZ { Start = _, _, zs; End = _, _, ze } = Math.Max(zs, ze)

    let cubes { Start = xs, ys, zs; End = xe, ye, ze } = [|
        let xMin, xMax = let sorted = List.sort [ xs; xe ] in sorted.[0], sorted.[1]
        let yMin, yMax = let sorted = List.sort [ ys; ye ] in sorted.[0], sorted.[1]
        let zMin, zMax = let sorted = List.sort [ zs; ze ] in sorted.[0], sorted.[1]

        for x in xMin .. xMax do
            for y in yMin .. yMax do
                for z in zMin .. zMax do
                    x, y, z
    |]

    let intersects b1 =
        let cs1 = b1 |> cubes |> set
        fun b2 -> b2 |> cubes |> Array.exists (fun (x, y, z) -> Set.contains (x, y, z) cs1)

    let crossSection brick =
        brick
        |> cubes
        |> Array.map (fun (x, y, _) -> x, y)
        |> Array.distinct

    let crossSectionsOverlap b1 =
        let cs1 = b1 |> crossSection |> set
        fun b2 -> b2 |> crossSection |> Array.exists (fun (x, y) -> Set.contains (x, y) cs1)

module SettledBricks =

    let supportedBricks settledBricks brick =

        let highestZ = Brick.highestZ brick
        let overlapsCrossSection = Brick.crossSectionsOverlap brick

        settledBricks |> Array.filter (fun b -> Brick.lowestZ b = highestZ + 1 && b |> overlapsCrossSection)

    let supportingBricks settledBricks brick =

        let lowestZ = Brick.lowestZ brick
        let overlapsCrossSection = Brick.crossSectionsOverlap brick

        settledBricks |> Array.filter (fun b -> Brick.highestZ b = lowestZ - 1 && b |> overlapsCrossSection)

    let filterToRemovable settledBricks =
        settledBricks
        |> Array.filter (fun brick ->
            let allSupportedBricksHaveOtherSupports =
                brick
                |> supportedBricks settledBricks
                |> Array.forall (supportingBricks settledBricks >> Array.length >> (<>) 1)

            allSupportedBricksHaveOtherSupports)

    let bricksThatWouldFallWhenDisintegrating settledBricks =

        let groupedSettledBricks =
            settledBricks
            |> Array.groupBy Brick.lowestZ
            |> Array.sortBy fst

        fun brick ->

            let wouldFall = HashSet<Brick>([| brick |])

            let z' = Brick.lowestZ brick
            let mutable highestRemovedCube = Brick.highestZ brick

            groupedSettledBricks
            |> Seq.skipWhile (fun (z, _) -> z <= z')
            |> Seq.map (fun (z, bricksAtZ) ->

                let wouldFallAtZ =
                    // Why does this not work with <= hRC + 1?!
                    if z <= highestRemovedCube + 3 then
                        bricksAtZ
                        |> Array.filter (supportingBricks settledBricks >> Array.forall wouldFall.Contains)
                    else
                        [||]

                wouldFallAtZ |> Array.iter (wouldFall.Add >> ignore)

                if wouldFallAtZ |> (not << Array.isEmpty) then
                    highestRemovedCube <- wouldFallAtZ |> Array.map Brick.highestZ |> Array.max

                z, wouldFallAtZ)
            |> Seq.toArray
            |> ignore

            wouldFall.Remove brick |> ignore

            wouldFall |> Seq.toArray |> Array.sortBy Brick.lowestZ

let parse =
    Array.map (fun (line: string) ->
        let parts = line.Split('~')
        let x0, y0, z0 =
            let tokens = parts.[0].Split(',')
            int tokens.[0], int tokens.[1], int tokens.[2]
        let x1, y1, z1 =
            let tokens = parts.[1].Split(',')
            int tokens.[0], int tokens.[1], int tokens.[2]
        { Start = x0, y0, z0; End = x1, y1, z1 })

let settle =
    Array.sortBy Brick.lowestZ
    >> Array.fold
        (fun state ({ Start = xs, ys, zs; End = xe, ye, ze } as brick) ->
            let overlapsCrossSection = Brick.crossSectionsOverlap brick
            let newZ =
                state
                |> List.filter overlapsCrossSection
                |> List.fold
                       (fun newZ b ->
                            match newZ, (Brick.highestZ b + 1) with
                            | Some newZ, z -> Some (max newZ z)
                            | None, z -> Some z
                        )
                       None
                |> Option.defaultValue 1
            if zs < ze then
                { Start = xs, ys, newZ; End = xe, ye, newZ + ze - zs } :: state
            else
                { Start = xs, ys, newZ + zs - ze; End = xe, ye, newZ } :: state)
        []
    >> List.toArray

module PartOne =

    let solve lines =
        lines
        |> parse
        |> settle
        |> SettledBricks.filterToRemovable
        |> Array.length

module PartTwo =

    let solve lines =

        let settledBricks =
            lines
            |> parse
            |> settle
            |> Array.sortBy Brick.lowestZ

        let bricksThatWouldFall = SettledBricks.bricksThatWouldFallWhenDisintegrating settledBricks

        (0, settledBricks)
        ||> Array.fold (fun state brick ->
            let wouldFall = brick |> bricksThatWouldFall |> Array.length
            wouldFall + state)

module Test =

    open Expecto
    open FsCheck

    module Brick =

        let tests = testList "Brick tests" [

            test "lowestZ" {
                let b = { Start = 1, 1, 3; End = 1, 1, 5 }

                let lowest = Brick.lowestZ b

                Expect.equal 3 lowest "lowest of 3 and 5 is 3"
            }

            test "highestZ" {
                let b = { Start = 1, 1, 3; End = 1, 1, 5 }

                let highest = Brick.highestZ b

                Expect.equal 5 highest "highest of 3 and 5 is 5"
            }

            test "cubes" {
                let b1 = { Start = 2, 3, 5; End = 2, 7, 5 }
                let b2 = { Start = 3, 2, 3; End = 1, 2, 3 }
                let b3 = { Start = 1, 1, 9; End = 1, 1, 1 }
                let b4 = { Start = 1, 1, 1; End = 1, 1, 1 }

                let cs1 = Brick.cubes b1
                let cs2 = Brick.cubes b2
                let cs3 = Brick.cubes b3
                let cs4 = Brick.cubes b4

                Expect.equal cs1 [| for y in 3 .. 7 do 2, y, 5 |] "cross section from 2, 3 to 2, 7"
                Expect.equal cs2 [| for x in 1 .. 3 do x, 2, 3 |] "cross section from 1, 2 to 3, 2"
                Expect.equal cs3 [| for z in 1 .. 9 do 1, 1, z |] "cross section from 1, 2 to 3, 2"
                Expect.equal cs4 [| 1, 1, 1 |] "cross section from 1, 1 to 1, 1"
            }


            testList "intersects" [

                test "returns false when no overlap" {
                    let b1 = { Start = 3, 1, 1; End = 3, 5, 1 }
                    let b2 = { Start = 5, 1, 2; End = 5, 5, 2 }

                    let result = Brick.intersects b1 b2

                    Expect.isFalse result "no intersection"
                }

                test "returns true when intersect perpendicularly" {
                    let b1 = { Start = 2, 2, 8; End = 2, 2, 2 }
                    let b2 = { Start = 3, 2, 5; End = 1, 2, 5 }

                    let result = Brick.intersects b1 b2

                    Expect.isTrue result "intersection"
                }

                test "returns true when intersect and part of same line" {
                    let b1 = { Start = 3, 3, 3; End = 3, 3, 6 }
                    let b2 = { Start = 3, 3, 8; End = 3, 3, 5 }

                    let result = Brick.intersects b1 b2

                    Expect.isTrue result "intersection"
                }

            ]

            test "crossSection" {
                let b1 = { Start = 2, 3, 5; End = 2, 7, 5 }
                let b2 = { Start = 3, 2, 3; End = 1, 2, 3 }
                let b3 = { Start = 1, 1, 1; End = 1, 1, 1 }

                let cs1 = Brick.crossSection b1
                let cs2 = Brick.crossSection b2
                let cs3 = Brick.crossSection b3

                Expect.equal cs1 [| 2, 3; 2, 4; 2, 5; 2, 6; 2, 7 |] "cross section from 2, 3 to 2, 7"
                Expect.equal cs2 [| 1, 2; 2, 2; 3, 2 |] "cross section from 1, 2 to 3, 2"
                Expect.equal cs3 [| 1, 1 |] "cross section from 1, 1 to 1, 1"
            }

            testList "crossSectionsOverlap" [

                test "returns false when no overlap" {
                    let b1 = { Start = 3, 1, 1; End = 3, 5, 1 }
                    let b2 = { Start = 5, 1, 2; End = 5, 5, 2 }

                    let result = Brick.crossSectionsOverlap b1 b2

                    Expect.isFalse result "no overlap"
                }

                test "returns true when single square overlaps" {
                    let b1 = { Start = 3, 1, 1; End = 3, 5, 1 }
                    let b2 = { Start = 1, 3, 2; End = 5, 3, 2 }

                    let result = Brick.crossSectionsOverlap b1 b2

                    Expect.isTrue result "overlap"
                }

                test "returns true when multiple squares overlap" {
                    let b1 = { Start = 3, 1, 1; End = 3, 5, 1 }
                    let b2 = { Start = 3, 3, 2; End = 3, 9, 2 }

                    let result = Brick.crossSectionsOverlap b1 b2

                    Expect.isTrue result "overlap"
                }

            ]

        ]

    module SettledBricks =

        let tests = testList "SettledBricks" [

            let a = { Start = 1, 0, 1; End = 1, 2, 1 }
            let b = { Start = 0, 0, 2; End = 2, 0, 2 }
            let c = { Start = 0, 2, 2; End = 2, 2, 2 }
            let d = { Start = 0, 0, 3; End = 0, 2, 3 }
            let e = { Start = 2, 0, 3; End = 2, 2, 3 }
            let f = { Start = 0, 1, 4; End = 2, 1, 4 }
            let g = { Start = 1, 1, 5; End = 1, 1, 6 }

            let settled = [| a; b; c; d; e; f; g |]

            test "supportedBricks" {
                let supportedBy brick = SettledBricks.supportedBricks settled brick

                Expect.equal (supportedBy a) [| b; c |] "a supports b and c"
                Expect.equal (supportedBy b) [| d; e |] "b supports d and e"
                Expect.equal (supportedBy c) [| d; e |] "c supports d and e"
                Expect.equal (supportedBy d) [| f |] "d supports f"
                Expect.equal (supportedBy e) [| f |] "e supports f"
                Expect.equal (supportedBy f) [| g |] "f supports g"
                Expect.equal (supportedBy g) [||] "g isn't supporting any bricks"
            }

            test "supportingBricks" {
                let supporting brick = SettledBricks.supportingBricks settled brick

                Expect.equal (supporting a) [||] "a is not supported by any bricks"
                Expect.equal (supporting b) [| a |] "b is supported by a"
                Expect.equal (supporting c) [| a |] "c is supported by a"
                Expect.equal (supporting d) [| b; c |] "d is supported by b and c"
                Expect.equal (supporting e) [| b; c |] "e is supported by b and c"
                Expect.equal (supporting f) [| d; e |] "f is supported by d and e"
                Expect.equal (supporting g) [| f |] "g is supported by f"
            }

            test "filterToRemovable" {
                let removable = SettledBricks.filterToRemovable settled

                Expect.equal removable [| b; c; d; e; g |] "b, c, d, e and g can be disintegrated"
            }

        ]

    type BrickGen() =

        static member private GenBrick =
            fun size -> gen {
                let genX = Gen.choose(0, size)
                let genY = Gen.choose(0, size)
                let genZ = Gen.choose(1, if size > 0 then size else 1)

                let! xs = genX
                let! ys = genY
                let! zs = genZ

                let! xe, ye, ze = gen {

                    match! Gen.choose(1, 3) with
                    | 1 ->
                        let! xe = genX
                        return xe, ys, zs
                    | 2 ->
                        let! ye = genY
                        return xs, ye, zs
                    | 3 ->
                        let! ze = genZ
                        return xs, ys, ze
                    | i ->
                        return failwith $"Bug: %i{i} is not between 1 and 3."

                }

                return { Start = xs, ys, zs; End = xe, ye, ze }
            }
            |> Gen.sized

        static member private GenBricks = gen {
            let! bricks = Gen.listOf BrickGen.GenBrick

            let filledCubes = HashSet<int * int * int>()

            return
                ([], bricks)
                ||> List.fold (fun state brick ->
                    let cubes = brick |> Brick.cubes

                    if filledCubes.Overlaps(cubes) then
                        state
                    else
                        cubes |> Array.iter (filledCubes.Add >> ignore)
                        brick :: state)
                |> List.toArray
        }

        static member private ShrinkBricks (bricks: Brick array) = seq {
            let l = bricks.Length
            for i in 0 .. l - 1 do
                    bricks |> Array.removeAt i
        }

        static member Brick () : Arbitrary<Brick> = Arb.fromGen BrickGen.GenBrick

        static member Bricks () : Arbitrary<Brick array> =
            Arb.fromGenShrink(BrickGen.GenBricks, BrickGen.ShrinkBricks)

    let fsCheckConfig = { FsCheckConfig.defaultConfig with arbitrary = [typeof<BrickGen>] }

    let settleTests = testList "settle" [

        let overlappingBricks bricks =
            (bricks, bricks)
            ||> Array.allPairs
            |> Array.filter (fun (b1, b2) -> b1 <> b2 && b1 |> Brick.intersects b2)

        let containsOverlappingBricks bricks =
            bricks
            |> overlappingBricks
            |> Array.isEmpty
            |> not

        testPropertyWithConfig fsCheckConfig "no overlapping bricks" (fun bricks ->
            Expect.isFalse (bricks |> containsOverlappingBricks) "Input should not have overlapping bricks"

            let settled = settle bricks

            let overlapping = settled |> overlappingBricks
            Expect.isEmpty overlapping $"Overlapping: %A{overlapping}")

    ]

    let all = testList "Day22" [
        Brick.tests
        SettledBricks.tests
        settleTests
    ]
