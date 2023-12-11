module aoc23.Day11

open FSharp.Stats

let (|UncheckedVec2|) (vec: Vector<float>) =
    let array = vec |> Vector.raw

    if array.Length <> 2 then
        failwith "provided vector is not of length 2"

    (array[0], array[1])

let manhattanDistance (UncheckedVec2(x1, y1)) (UncheckedVec2(x2, y2)) = abs (x1 - x2) + abs (y1 - y2)

type Space(lines: string array) =
    let matrix =
        lines
        |> Array.map (fun line -> line |> Seq.map (fun c -> if c = '#' then 1.0 else 0.0) |> Seq.toArray)
        |> matrix

    let expansionPoints =
        let helper mapping =
            matrix
            |> mapping
            |> Vector.raw
            |> Array.indexed
            |> Array.filter (snd >> ((=) 0.0))
            |> Array.map fst

        (Matrix.sumColumns >> RowVector.transpose, Matrix.sumRows) |> TupleEx.map helper

    let galaxies =
        ([], matrix)
        ||> Matrix.foldi (fun y x galaxies ->
            function
            | 1.0 -> vector [| float x; float y |] :: galaxies
            | _ -> galaxies)
    
    member self.MapPointToExpanded expansionLevel (UncheckedVec2(x, y)) =
        let expansionX, expansionY = expansionPoints

        [| expansionX, x; expansionY, y |]
        |> Array.map (fun (expansion, value) ->
            expansion
            |> Seq.filter (fun e -> (float e) < value)
            |> Seq.length
            |> (fun e ->  value + float e * expansionLevel))
            |> vector

    member self.Galaxies = galaxies

let rec allPairs results =
    function
    | [] -> []
    | [ _ ] -> results
    | cur :: remaining ->
        let pairs = remaining |> List.map (fun n -> (cur, n))
        allPairs (pairs @ results) remaining

let solve expansion lines =
    let space = Space lines
    
    space.Galaxies
    |> List.map (space.MapPointToExpanded expansion)
    |> allPairs []
    |> Seq.map (fun (a, b) -> manhattanDistance a b)
    |> Seq.sum

let part1 = solve 1

let part2 = solve 999_999

let run = runReadAllLines part1 part2

module Tests =
    open Xunit
    open Swensen.Unquote

    let example1 =
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
    let ``part 1 example 1`` () = part1 example1 =! 374
    
    [<Fact>]
    let ``part 2 example 10`` () = solve 9 example1 =! 1030
    
    [<Fact>]
    let ``part 2 example 100`` () = solve 99 example1 =! 8410
