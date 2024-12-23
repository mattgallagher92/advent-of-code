module Day9

type Block =
    | FileBlock of int64
    | FreeSpace

let parse (input: string) =
    let diskMap = input |> Seq.toArray |> Array.map (_.ToString() >> int)
    let numBlocks = diskMap |> Array.sum
    let disk = Array.create numBlocks FreeSpace

    let mutable i = 0
    let mutable fileIndex = 0
    let mutable isFile = true

    for digit in diskMap do
        if isFile then
            Array.fill disk i digit (FileBlock fileIndex)
            fileIndex <- fileIndex + 1

        i <- i + digit
        isFile <- not isFile

    disk

let checksum disk =
    disk |> Array.indexed |> Array.sumBy (fun (i, f: int64) -> int64 i * f)

module PartOne =

    let defragment fragmentedDisk =
        let numFileBlocks =
            fragmentedDisk
            |> Array.sumBy (function
                | FileBlock _ -> 1
                | FreeSpace -> 0)

        let defragmentedDisk = Array.create numFileBlocks 0L

        let mutable i = 0
        let mutable j = fragmentedDisk.Length - 1

        while i <= j do
            match fragmentedDisk[i], fragmentedDisk[j] with
            | FreeSpace, FreeSpace -> j <- j - 1
            | FreeSpace, FileBlock fbj ->
                defragmentedDisk[i] <- fbj
                i <- i + 1
                j <- j - 1
            | FileBlock fbi, FreeSpace ->
                defragmentedDisk[i] <- fbi
                i <- i + 1
                j <- j - 1
            | FileBlock fbi, FileBlock _ ->
                defragmentedDisk[i] <- fbi
                i <- i + 1

        defragmentedDisk

    let solve (input: string) =
        input |> parse |> defragment |> checksum

module PartTwo =

    let solve (input: string) = -1

module Test =

    open Expecto
    open Swensen.Unquote

    let all =
        testList "Day9" [
            let sampleInput = "2333133121414131402"

            let sampleDisk = [|
                FileBlock 0
                FileBlock 0
                FreeSpace
                FreeSpace
                FreeSpace
                FileBlock 1
                FileBlock 1
                FileBlock 1
                FreeSpace
                FreeSpace
                FreeSpace
                FileBlock 2
                FreeSpace
                FreeSpace
                FreeSpace
                FileBlock 3
                FileBlock 3
                FileBlock 3
                FreeSpace
                FileBlock 4
                FileBlock 4
                FreeSpace
                FileBlock 5
                FileBlock 5
                FileBlock 5
                FileBlock 5
                FreeSpace
                FileBlock 6
                FileBlock 6
                FileBlock 6
                FileBlock 6
                FreeSpace
                FileBlock 7
                FileBlock 7
                FileBlock 7
                FreeSpace
                FileBlock 8
                FileBlock 8
                FileBlock 8
                FileBlock 8
                FileBlock 9
                FileBlock 9
            |]

            let sampleDefragmentedDisk = [|
                0L
                0L
                9L
                9L
                8L
                1L
                1L
                1L
                8L
                8L
                8L
                2L
                7L
                7L
                7L
                3L
                3L
                3L
                6L
                4L
                4L
                6L
                5L
                5L
                5L
                5L
                6L
                6L
            |]

            testCase "parse works with sample input" (fun _ -> test <@ parse sampleInput = sampleDisk @>)

            testList "PartOne" [
                testCase "defragment works with sample input" (fun _ ->
                    test <@ PartOne.defragment sampleDisk = sampleDefragmentedDisk @>)

                testCase "solve works with sample input" (fun _ -> test <@ PartOne.solve sampleInput = 1928 @>)
            ]

            testList "PartTwo" [
                testCase "solve works with sample input" (fun _ -> test <@ PartTwo.solve sampleInput = -1 @>)
            ]
        ]

let dayFns = {
    Tests = Test.all
    UtilTests = []
    PartOne = Array.head >> PartOne.solve >> int64
    PartTwo = Array.head >> PartTwo.solve >> int64
}
