[<AutoOpen>]
module Advent

type DayFunctions = {
    Tests: Expecto.Test
    UtilTests: Expecto.Test list
    PartOne: string array -> int64
    PartTwo: string array -> int64
}
