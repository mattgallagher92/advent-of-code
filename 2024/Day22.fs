module Day22

let mix secret v = secret ^^^ v

let prune x =
    // 16777216 is 2^24
    x &&& 0b111_111_111_111_111_111_111_111u

let evolve (secret: uint32) =
    let applyMixAndPrune f secret = secret |> f |> mix secret |> prune

    secret
    // 64 is 2^6
    |> applyMixAndPrune (fun x -> x <<< 6)
    // 32 is 2^5
    |> applyMixAndPrune (fun x -> x >>> 5)
    // 2048 is 2^11
    |> applyMixAndPrune (fun x -> x <<< 11)

module PartOne =

    let solve (lines: string array) =
        let evolve2000 = evolve |> Array.replicate 2000 |> Array.reduce (>>)

        lines |> Array.sumBy (uint32 >> evolve2000 >> int64)

module PartTwo =

    let findBestFourDeltaAndTotalPrice n secrets =

        let p secret = secret % 10u |> int8

        /// Stores running total price across all monkeys for encountered 4-deltas.
        let globalD = System.Collections.Generic.Dictionary<_, int64>()

        for secret in secrets do

            let mutable prev = secret

            // For tracking 4-deltas. Initialised to meaningless values.
            let mutable diff''' = 127y
            let mutable diff'' = 127y
            let mutable diff' = 127y
            let mutable diff = 127y

            /// Stores first price for 4-deltas that the monkey with the given secret encounters.
            let monkeyD = System.Collections.Generic.Dictionary<_, _>()

            for i in 1..n do

                let next = evolve prev
                let price = p next

                diff''' <- diff''
                diff'' <- diff'
                diff' <- diff
                diff <- price - p prev
                prev <- next

                // So that we have enough actual diffs (note that i starts from 1).
                if i > 3 then

                    let fourDelta = struct (diff''', diff'', diff', diff)

                    // The negotiation stops the first time a 4-delta matches the instruction.
                    if not (monkeyD.ContainsKey fourDelta) then
                        monkeyD[fourDelta] <- price

            // Add the first encountered price for each 4-delta encountered by the monkey to the running totals.
            monkeyD
            |> Seq.iter (fun kvp ->
                let fourDelta, price = kvp.Key, int64 kvp.Value

                globalD.TryGet fourDelta
                |> Option.defaultValue 0L
                |> (+) price
                |> fun newTotal -> globalD[fourDelta] <- newTotal)

        // Find the highest total price and return the 4-delta with it.
        globalD |> Seq.maxBy _.Value |> (fun kvp -> kvp.Key, kvp.Value)

    let solve (lines: string array) =
        lines |> Array.map uint32 |> findBestFourDeltaAndTotalPrice 2000 |> snd

module Test =

    open Expecto
    open Swensen.Unquote

    let all =
        testList "Day22" [
            let sampleInput = [| "1"; "10"; "100"; "2024" |]

            testCase "prune 100000000 = 16113920" (fun _ -> test <@ prune 100000000u = 16113920u @>)

            testCase "successive evolution of 123 works" (fun _ ->
                test
                    <@
                        (123u, [| 1..10 |])
                        ||> Array.scan (fun prev _ -> evolve prev)
                        |> Array.skip 1
                        |> (=) [|
                            15887950u
                            16495136u
                            527345u
                            704524u
                            1553684u
                            12683156u
                            11100544u
                            12249484u
                            7753432u
                            5908254u
                        |]
                    @>)

            testList "PartOne" [
                testCase "solve works with sample input" (fun _ -> test <@ PartOne.solve sampleInput = 37327623 @>)
            ]

            testList "PartTwo" [
                testCase "findBestFourDeltaAndTotalPrice works for example monkey" (fun _ ->
                    test <@ PartTwo.findBestFourDeltaAndTotalPrice 10 [ 123u ] = (struct (-1y, -1y, 0y, 2y), 6L) @>)

                testCase "findBestFourDeltaAndTotalPrice works for four example monkeys" (fun _ ->
                    test
                        <@
                            let result = PartTwo.findBestFourDeltaAndTotalPrice 2000 [ 1u; 2u; 3u; 2024u ]
                            result = (struct (-2y, 1y, -1y, 3y), 23L)
                        @>)
            ]
        ]

let dayFns = {
    Tests = Test.all
    UtilTests = []
    PartOne = PartOne.solve >> int64
    PartTwo = PartTwo.solve >> int64
}
