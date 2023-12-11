module aoc23.Day11

open FSharp.Stats

type Space(lines: string array) =
    let matrix =
        lines
        |> Array.map (
            Seq.map (function
                | '#' -> 1.0
                | _ -> 0.0)
            >> Seq.toArray
        )
        |> matrix

    let expansionPoints =
        let emptyIndices matrixMapping =
            matrix
            |> matrixMapping
            |> Vector.raw
            |> Array.indexed
            |> Array.filter (snd >> ((=) 0.0))
            |> Array.map fst

        [| emptyIndices (Matrix.sumColumns >> RowVector.transpose)
           emptyIndices Matrix.sumRows |]

    let galaxies =
        ([], matrix)
        ||> Matrix.foldi (fun y x galaxies ->
            function
            | 1.0 -> vector [| float x; float y |] :: galaxies
            | _ -> galaxies)

    member self.MapPointToExpanded expansionLevel =
        Vector.raw
        >> Array.zip expansionPoints
        >> Array.map (fun (expansion, value) ->
            expansion
            |> Seq.filter (fun e -> (float e) < value)
            |> Seq.length
            |> (fun e -> value + float e * (expansionLevel - 1.0)))
        >> vector

    member self.Galaxies = galaxies

let allPairs list =
    let rec inner previous =
        function
        | cur :: (_ :: _ as remaining) ->
            let next = remaining |> List.map (fun n -> (cur, n))
            inner (next @ previous) remaining
        | _ -> previous

    inner [] list

let manhattanDistance vec1 vec2 =
    (vec1 - vec2) |> Vector.map abs |> Vector.sum

let solve expansion lines =
    let space = Space lines

    space.Galaxies
    |> List.map (space.MapPointToExpanded expansion)
    |> allPairs
    |> Seq.sumBy (TupleEx.apply manhattanDistance)

let part1 = solve 2
let part2 = solve 1_000_000
let run = runReadAllLines part1 part2

module Tests =
    open Xunit
    open Swensen.Unquote

    let example =
        [| "...#......"
           ".......#.."
           "#........."
           ".........."
           "......#..."
           ".#........"
           ".........#"
           ".........."
           ".......#.."
           "#...#....." |]

    [<Fact>]
    let ``part 1 example 1`` () = part1 example =! 374

    [<Fact>]
    let ``part 2 example 10`` () = solve 10 example =! 1030

    [<Fact>]
    let ``part 2 example 100`` () = solve 100 example =! 8410
