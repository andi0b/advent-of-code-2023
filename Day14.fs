module aoc23.Day14

open System
open FSharpAux

let parse lines = lines |> array2D

let rec moveLeft line =
    let processSegment segment =
        let stoneCount =
            segment
            |> Array.countBy id
            |> Map
            |> (fun counts ->
                if counts |> Map.containsKey '#' then
                    failwith "no # expected!"

                counts |> Map.tryFind 'O' |> Option.defaultValue 0)

        Seq.init segment.Length (fun i -> if i < stoneCount then 'O' else '.')

    [| match line |> Array.tryFindIndex ((=) '#') with
       | Some index ->
           let segment, remaining = line |> Array.splitAt index
           yield! processSegment segment
           yield '#'
           yield! moveLeft remaining[1..]
       | None -> yield! processSegment line |]

let moveNorth grid =
    [| for col in 0 .. (Array2D.colCount grid) - 1 do
           moveLeft grid[*, col] |]
    |> array2D
    |> Array2D.transpose

let calculateWeight grid =
    let rows = grid |> Array2D.rowCount

    grid
    |> Array2D.mapRowI (fun row ->
        function
        | 'O' -> rows - row
        | _ -> 0)
    |> Array2D.array2D_to_seq
    |> Seq.sum

let turnClockwise source =
    let rowCount, colCount = source |> Array2D.rowCount, source |> Array2D.colCount
    Array2D.init colCount rowCount (fun row col -> source[rowCount - 1 - col, row])

let doCycle grid =
    let times i f x =
        (seq { 1..i }) |> Seq.fold (fun acc _ -> f acc) x

    times 4 (moveNorth >> turnClockwise) grid

let findCycleLoop grid =

    // let's just be bold and assume that after 100 iterations the first repeated weight measure with at least
    // one item in between forms a cycle, and just emit that. seems to work :D
    let rec loop cycle previousResults grid =

        let cycledGrid = doCycle grid
        let weight = cycledGrid |> calculateWeight

        let firstIdx = previousResults |> List.tryFindIndex ((=) weight)

        let secondIndex =
            firstIdx
            |> Option.bind (fun i -> previousResults |> List.skip (i + 1) |> List.tryFindIndex ((=) weight))

        match (firstIdx, secondIndex) with
        | Some f, Some s when s = f && s > 1  && cycle > 100 ->

            {| loop = previousResults[0..f] |> List.rev
               offset = cycle |}

        | _ -> loop (cycle + 1) (weight :: previousResults) cycledGrid

    loop 1 [] grid



let part1 lines =
    parse lines |> moveNorth |> calculateWeight


let part2 lines =
    let cycleLoop = lines |> parse |> findCycleLoop
    cycleLoop.loop[(1_000_000_000 - cycleLoop.offset) % cycleLoop.loop.Length]

let run = runReadAllLines part1 part2

module Tests =
    open Xunit
    open Swensen.Unquote

    let example =
        [| "O....#...."
           "O.OO#....#"
           ".....##..."
           "OO.#O....O"
           ".O.....O#."
           "O.#..O.#.#"
           "..O..#O..O"
           ".......O.."
           "#....###.."
           "#OO..#...." |]

    [<Fact>]
    let ``move left`` () =

        let testHarness str =
            str |> Array.ofSeq |> moveLeft |> String

        test <@ "..O..#O..O" |> testHarness = "O....#OO.." @>

    [<Fact>]
    let ``part 1 example`` () = part1 example =! 136

    [<Fact>]
    let ``part 2 example`` () = part2 example =! 64


    [<Fact>]
    let ``turn clockwise`` () =
        array2D
            [ [ 1; 2; 3 ] //
              [ 3; 4; 5 ] ]
        |> turnClockwise
        =! array2D
            [ [ 3; 1 ] //
              [ 4; 2 ]
              [ 5; 3 ] ]