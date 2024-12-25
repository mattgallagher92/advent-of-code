module Day9

type Block =
    | FileBlock of int64
    | FreeSpace

module PartOne =

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

    let checksum disk =
        disk |> Array.indexed |> Array.sumBy (fun (i, f: int64) -> int64 i * f)

    let solve (input: string) =
        input |> parse |> defragment |> checksum

type FileDiskLayout = {
    FileId: int
    FileSize: int
    FollowingFreeSpace: int
} with

    member this.FileIdL = int64 this.FileId
    member this.FileSizeL = int64 this.FileSize
    member this.TotalBlocks = this.FileSize + this.FollowingFreeSpace
    member this.TotalBlocksL = int64 this.TotalBlocks

module PartTwo =

    let parse (input: string) =
        let diskMap = input |> Seq.toArray |> Array.map (_.ToString() >> int)

        [|
            for fileBlockIx in
                seq {
                    for i in 0 .. diskMap.Length - 1 do
                        if i % 2 = 0 then i else ()
                } do
                {
                    FileId = fileBlockIx / 2
                    FileSize = diskMap[fileBlockIx]
                    FollowingFreeSpace =
                        if fileBlockIx < diskMap.Length - 1 then
                            diskMap[fileBlockIx + 1]
                        else
                            0
                }
        |]

    let defragFile idToMove layout =
        let ix = layout |> Array.findIndex (_.FileId >> (=) idToMove)
        let file = layout[ix]

        match
            layout
            |> Array.take ix
            |> Array.tryFindIndex (fun f -> f.FollowingFreeSpace >= file.FileSize)
            |> Option.map ((+) 1)
        with
        | Some ixToMoveTo when ixToMoveTo = ix ->
            let newLayout = Array.copy layout

            newLayout[ix - 1] <- { layout[ix - 1] with FollowingFreeSpace = 0 }

            newLayout[ix] <- {
                layout[ix] with
                    FollowingFreeSpace = layout[ix].FollowingFreeSpace + layout[ix - 1].FollowingFreeSpace
            }

            newLayout
        | Some ixToMoveTo ->
            let newLayout = Array.copy layout
            Array.blit layout ixToMoveTo newLayout (ixToMoveTo + 1) (ix - ixToMoveTo)

            newLayout[ixToMoveTo - 1] <- { layout[ixToMoveTo - 1] with FollowingFreeSpace = 0 }

            newLayout[ixToMoveTo] <- {
                file with
                    FollowingFreeSpace = layout[ixToMoveTo - 1].FollowingFreeSpace - file.FileSize
            }

            newLayout[ix] <- {
                layout[ix - 1] with
                    FollowingFreeSpace = layout[ix - 1].FollowingFreeSpace + file.TotalBlocks
            }

            newLayout
        | None -> layout

    let defragment (diskLayout: FileDiskLayout array) =
        let rec inner idToMove layout =
            if idToMove = -1 then
                layout
            else
                defragFile idToMove layout |> inner (idToMove - 1)

        (diskLayout |> Array.last |> _.FileId, diskLayout) ||> inner

    let checksum (diskLayout: FileDiskLayout array) =
        ((0L, 0L), diskLayout)
        ||> Array.fold (fun (currentBlock, currentSum) (fdl: FileDiskLayout) ->
            // currentBlock * fileId
            // + (currentBlock + 1) * fileId
            // + (currentBlock + 2) * fileId
            // + ...
            // + (currentBlock + fileSize - 1) * fileId
            let fileContribution =
                (currentBlock * fdl.FileSizeL + fdl.FileSizeL * (fdl.FileSizeL - 1L) / 2L)
                * fdl.FileIdL

            currentBlock + fdl.TotalBlocksL, currentSum + fileContribution)
        |> snd

    let solve (input: string) =
        input |> parse |> defragment |> checksum

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

            testList "PartOne" [
                testCase "parse works with sample input" (fun _ -> test <@ PartOne.parse sampleInput = sampleDisk @>)

                testCase "defragment works with sample input" (fun _ ->
                    test <@ PartOne.defragment sampleDisk = sampleDefragmentedDisk @>)

                testCase "solve works with sample input" (fun _ -> test <@ PartOne.solve sampleInput = 1928 @>)
            ]

            testList "PartTwo" [
                let sampleLayout = [|
                    { FileId = 0; FileSize = 2; FollowingFreeSpace = 3 }
                    { FileId = 1; FileSize = 3; FollowingFreeSpace = 3 }
                    { FileId = 2; FileSize = 1; FollowingFreeSpace = 3 }
                    { FileId = 3; FileSize = 3; FollowingFreeSpace = 1 }
                    { FileId = 4; FileSize = 2; FollowingFreeSpace = 1 }
                    { FileId = 5; FileSize = 4; FollowingFreeSpace = 1 }
                    { FileId = 6; FileSize = 4; FollowingFreeSpace = 1 }
                    { FileId = 7; FileSize = 3; FollowingFreeSpace = 1 }
                    { FileId = 8; FileSize = 4; FollowingFreeSpace = 0 }
                    { FileId = 9; FileSize = 2; FollowingFreeSpace = 0 }
                |]

                testCase "parse works with sample input" (fun _ -> test <@ PartTwo.parse sampleInput = sampleLayout @>)

                testCase "defragFile works with sample input" (fun _ ->
                    let expected = [|
                        { FileId = 0; FileSize = 2; FollowingFreeSpace = 0 }
                        { FileId = 9; FileSize = 2; FollowingFreeSpace = 1 }
                        { FileId = 1; FileSize = 3; FollowingFreeSpace = 3 }
                        { FileId = 2; FileSize = 1; FollowingFreeSpace = 3 }
                        { FileId = 3; FileSize = 3; FollowingFreeSpace = 1 }
                        { FileId = 4; FileSize = 2; FollowingFreeSpace = 1 }
                        { FileId = 5; FileSize = 4; FollowingFreeSpace = 1 }
                        { FileId = 6; FileSize = 4; FollowingFreeSpace = 1 }
                        { FileId = 7; FileSize = 3; FollowingFreeSpace = 1 }
                        { FileId = 8; FileSize = 4; FollowingFreeSpace = 2 }
                    |]

                    test <@ PartTwo.defragFile 9 sampleLayout = expected @>)

                testCase "defragment works with sample input" (fun _ ->
                    let expected = [|
                        { FileId = 0; FileSize = 2; FollowingFreeSpace = 0 }
                        { FileId = 9; FileSize = 2; FollowingFreeSpace = 0 }
                        { FileId = 2; FileSize = 1; FollowingFreeSpace = 0 }
                        { FileId = 1; FileSize = 3; FollowingFreeSpace = 0 }
                        { FileId = 7; FileSize = 3; FollowingFreeSpace = 1 }
                        { FileId = 4; FileSize = 2; FollowingFreeSpace = 1 }
                        { FileId = 3; FileSize = 3; FollowingFreeSpace = 4 }
                        { FileId = 5; FileSize = 4; FollowingFreeSpace = 1 }
                        { FileId = 6; FileSize = 4; FollowingFreeSpace = 5 }
                        { FileId = 8; FileSize = 4; FollowingFreeSpace = 2 }
                    |]

                    test <@ PartTwo.defragment sampleLayout = expected @>)

                testCase "solve works with sample input" (fun _ -> test <@ PartTwo.solve sampleInput = 2858L @>)

                // Example from https://www.reddit.com/r/adventofcode/comments/1haa13a/comment/m16z9nc/. Catches case
                // where a file doesn't change order relative to other files, but does move next to the previous file.
                let fixedFileSampleInput = "1313165"

                let fixedFileDiskLayout = [|
                    { FileId = 0; FileSize = 1; FollowingFreeSpace = 3 }
                    { FileId = 1; FileSize = 1; FollowingFreeSpace = 3 }
                    { FileId = 2; FileSize = 1; FollowingFreeSpace = 6 }
                    { FileId = 3; FileSize = 5; FollowingFreeSpace = 0 }
                |]

                testCase "parse works with fixedFile sample" (fun _ ->
                    test <@ PartTwo.parse fixedFileSampleInput = fixedFileDiskLayout @>)

                testCase "defragFile works with fixedFile disk layout" (fun _ ->
                    let expected = [|
                        { FileId = 0; FileSize = 1; FollowingFreeSpace = 3 }
                        { FileId = 1; FileSize = 1; FollowingFreeSpace = 3 }
                        { FileId = 2; FileSize = 1; FollowingFreeSpace = 0 }
                        { FileId = 3; FileSize = 5; FollowingFreeSpace = 6 }
                    |]

                    test <@ PartTwo.defragFile 3 fixedFileDiskLayout = expected @>)

                testCase "defragment works with fixedFile disk layout" (fun _ ->
                    let expected = [|
                        { FileId = 0; FileSize = 1; FollowingFreeSpace = 0 }
                        { FileId = 2; FileSize = 1; FollowingFreeSpace = 0 }
                        { FileId = 1; FileSize = 1; FollowingFreeSpace = 6 }
                        { FileId = 3; FileSize = 5; FollowingFreeSpace = 6 }
                    |]

                    test <@ PartTwo.defragment fixedFileDiskLayout = expected @>)

                testCase "solve works with fixedFile sample" (fun _ ->
                    test <@ PartTwo.solve fixedFileSampleInput = 169L @>)
            ]
        ]

let dayFns = {
    Tests = Test.all
    UtilTests = []
    PartOne = Array.head >> PartOne.solve
    PartTwo = Array.head >> PartTwo.solve
}
