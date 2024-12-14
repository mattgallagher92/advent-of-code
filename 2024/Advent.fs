[<AutoOpen>]
module Advent

type DayFunctions<'input> = {
    Tests: Expecto.Test
    ReadInput: string -> 'input
    PartOne: 'input -> int
    PartTwo: 'input -> int
}
