module Day22

module PartOne =

    let mix secret v = secret ^^^ v

    let prune x =
        // 16777216 is 2^24
        x &&& 0b111_111_111_111_111_111_111_111UL

    let evolve (secret: uint64) =
        let applyMixAndPrune f secret = secret |> f |> mix secret |> prune

        secret
        // 64 is 2^6
        |> applyMixAndPrune (fun x -> x <<< 6)
        // 32 is 2^5
        |> applyMixAndPrune (fun x -> x >>> 5)
        // 2048 is 2^11
        |> applyMixAndPrune (fun x -> x <<< 11)

    let solve (lines: string array) =
        let evolve2000 = evolve |> Array.replicate 2000 |> Array.reduce (>>)

        lines |> Array.sumBy (uint64 >> evolve2000) |> int64

module PartTwo =

    let solve (lines: string array) = -1

module Test =

    open Expecto
    open Swensen.Unquote

    let all =
        testList "DayN" [
            let sampleInput = [| "1"; "10"; "100"; "2024" |]

            testList "PartOne" [
                testCase "prune 100000000 = 16113920" (fun _ -> test <@ PartOne.prune 100000000UL = 16113920UL @>)

                testCase "successive evolution of 123 works" (fun _ ->
                    test
                        <@
                            (123UL, [| 1..10 |])
                            ||> Array.scan (fun prev _ -> PartOne.evolve prev)
                            |> Array.skip 1
                            |> (=) [|
                                15887950UL
                                16495136UL
                                527345UL
                                704524UL
                                1553684UL
                                12683156UL
                                11100544UL
                                12249484UL
                                7753432UL
                                5908254UL
                            |]
                        @>)

                testCase "solve works with sample input" (fun _ -> test <@ PartOne.solve sampleInput = 37327623 @>)
            ]

            testList "PartTwo" [
                testCase "solve works with sample input" (fun _ -> test <@ PartTwo.solve sampleInput = -1 @>)
            ]
        ]

let dayFns = {
    Tests = Test.all
    UtilTests = []
    PartOne = PartOne.solve >> int64
    PartTwo = PartTwo.solve >> int64
}
