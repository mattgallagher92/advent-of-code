module Day13

open System

module PartOne =

    let hasHorizontalLineOfSymmetryBeforeRow n pattern =

        let height = pattern |> Array2D.length1
        let numRowPairsToTest = Math.Min(height - 1 - n, n - 1)

        seq { 0 .. numRowPairsToTest }
        |> Seq.forall (fun i -> Array2D.row (n - 1 - i) pattern = Array2D.row (n + i) pattern)

    let hasVerticalLineOfSymmetryBeforeColumn n pattern =

        let width = pattern |> Array2D.length2
        let numColumnPairsToTest = Math.Min(width - 1 - n, n - 1)

        seq { 0 .. numColumnPairsToTest }
        |> Seq.forall (fun j -> Array2D.column (n - 1 - j) pattern = Array2D.column (n + j) pattern)

    let solve (text: string) =
        text.Split("\n\n")
        |> Array.map (fun patternStr ->
            patternStr.Split('\n') |> Array.filter (not << String.IsNullOrWhiteSpace) |> array2D)
        |> Array.map (fun pattern ->
            // Start at 1 because before row 0 would pass the test but have no reflected rows.
            seq { 1 .. Array2D.length1 pattern - 1 }
            |> Seq.tryFind (fun n -> pattern |> hasHorizontalLineOfSymmetryBeforeRow n)
            |> Option.map (fun n -> n * 100)
            |> Option.defaultWith (fun _ ->
                seq { 1 .. Array2D.length2 pattern - 1 }
                |> Seq.find (fun n -> pattern |> hasVerticalLineOfSymmetryBeforeColumn n)))
        |> Array.sum

module PartTwo =

    let private hasExactlyOneFalse seq =
        seq
        // Generate running false total.
        |> Seq.scan (fun running testResult -> if testResult then running else running + 1) 0
        // Stop if running total reaches 2 (to avoid unnecessary work).
        |> Seq.takeUntilFirstMatch ((=) 2)
        |> Seq.last
        // It is a candidate if there is only one mismatch.
        |> (=) 1

    let hasSmudgedHorizontalLineOfSymmetryBeforeRow n pattern =

        let height, width = pattern |> Array2D.length1, pattern |> Array2D.length2
        let numRowPairsToTest = Math.Min(height - 1 - n, n - 1)

        seq { 0 .. numRowPairsToTest }
        // Generate sequence of results from testing whether corresponding elements across line being tested for
        // symmetry are equal.
        |> Seq.collect (fun i ->
            seq { 0 .. width - 1 }
            |> Seq.map (fun j -> Array2D.get pattern (n - 1 - i) j = Array2D.get pattern (n + i) j))
        |> hasExactlyOneFalse

    let hasSmudgedVerticalLineOfSymmetryBeforeColumn n pattern =

        let height, width = pattern |> Array2D.length1, pattern |> Array2D.length2
        let numColumnPairsToTest = Math.Min(width - 1 - n, n - 1)

        seq { 0 .. numColumnPairsToTest }
        // Generate sequence of results from testing whether corresponding elements across line being tested for
        // symmetry are equal.
        |> Seq.collect (fun j ->
            seq { 0 .. height - 1 }
            |> Seq.map (fun i -> Array2D.get pattern i (n - 1 - j) = Array2D.get pattern i (n + j)))
        |> hasExactlyOneFalse

    let solve (text: string) =
        text.Split("\n\n")
        |> Array.map (fun patternStr ->
            patternStr.Split('\n') |> Array.filter (not << String.IsNullOrWhiteSpace) |> array2D)
        |> Array.map (fun pattern ->
            // Start at 1 because before row 0 would pass the test but have no reflected rows.
            seq { 1 .. Array2D.length1 pattern - 1 }
            |> Seq.tryFind (fun n -> pattern |> hasSmudgedHorizontalLineOfSymmetryBeforeRow n)
            |> Option.map (fun n -> n * 100)
            |> Option.defaultWith (fun _ ->
                seq { 1 .. Array2D.length2 pattern - 1 }
                |> Seq.find (fun n -> pattern |> hasSmudgedVerticalLineOfSymmetryBeforeColumn n)))
        |> Array.sum

