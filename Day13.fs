module aoc23.Day13

open FSharp.Stats

type MirrorPoint =
    | Row of int
    | Col of int

module MirrorPoint =
    let value =
        function
        | Col c -> c
        | Row r -> r * 100

type Grid(input) =
    let matrix =
        let lines = input |> StringEx.splitSs [| "\r\n"; "\n" |]
        let rows = lines.Length
        let cols = lines[0].Length
        Matrix.init rows cols (fun r c -> if lines[r][c] = '#' then 1.0 else 0.0)

    let rows =
        [| 0 .. matrix.NumRows - 1 |]
        |> Array.map (Matrix.getRow matrix >> RowVector.transpose)

    let cols = [| 0 .. matrix.NumCols - 1 |] |> Array.map (Matrix.getCol matrix)

    let findMirrorPoint comparer =
        seq {
            for r in 1 .. matrix.NumRows - 1 do
                if comparer rows r then
                    yield Row r

            for c in 1 .. matrix.NumCols - 1 do
                if comparer cols c then
                    yield Col c
        }
        |> Seq.exactlyOne

    member self.MirrorPoint =
        let isMirrorAt (array: 'a array) idx =
            let first, second = (array[.. idx - 1], array[idx..])
            (first |> Seq.rev, second) ||> Seq.forall2 (=)

        findMirrorPoint isMirrorAt

    member self.UnsmugedMirrorPoint =
        let isMirrorWithExactlyOneSmudge (array: vector array) idx =
            let first, second = (array[.. idx - 1], array[idx..])

            (first |> Seq.rev, second)
            ||> Seq.map2 (fun a b -> (a - b) |> Vector.map abs |> Vector.sum)
            |> Seq.sum = 1.0

        findMirrorPoint isMirrorWithExactlyOneSmudge

    static member ParseMany input =
        let parts = input |> StringEx.splitSs [| "\r\n\r\n"; "\n\n" |]
        parts |> Array.map Grid

let solve mapper input =
    let grids = Grid.ParseMany input

    grids |> Seq.map (fun g -> g |> mapper |> MirrorPoint.value) |> Seq.sum

let part1 = solve _.MirrorPoint
let part2 = solve _.UnsmugedMirrorPoint
let run = runReadAllText part1 part2

module Tests =
    open Xunit
    open Swensen.Unquote

    let example =
        """#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#"""

    [<Fact>]
    let ``part 1 example`` () = part1 example =! 405

    [<Fact>]
    let ``part 2 example`` () = part2 example =! 400
