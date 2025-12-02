[<AutoOpen>]
module Util

open System

let dbg label x =
    printfn $"%s{label}: %A{x}"
    x

[<RequireQualifiedAccess>]
module Int32 =
    let tryParse (s: string) =
        match Int32.TryParse s with
        | true, i -> Some i
        | false, _ -> None

[<RequireQualifiedAccess>]
module Math =
    let integerPower base' exponent =
        if exponent < 0 then
            invalidArg (nameof exponent) $"exponent must be at least 0; given %i{exponent}"
        else
            Seq.replicate exponent base' |> Seq.fold (*) 1L
