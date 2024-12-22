[<AutoOpen>]
module Advent

type DayFunctions<'input> = {
    Tests: Expecto.Test
    ReadInput: string -> 'input
    PartOne: 'input -> int64
    PartTwo: 'input -> int64
}
