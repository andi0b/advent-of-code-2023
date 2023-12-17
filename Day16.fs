module aoc23.Day16

open System.Collections.Generic
open System.Threading.Tasks

[<Struct>]
type Direction =
    | N
    | S
    | E
    | W

type Point = (struct (int * int))

module Point =
    let add struct (a1, b1) struct (a2, b2) = struct (a1 + a2, b1 + b2)

// really need a simple struct here to have fast Equals/GetHashCode on the hot path
[<Struct>]
type Cursor =
    val row: int
    val col: int
    val direction: Direction

    new(struct (row, col), direction) =
        { row = row
          col = col
          direction = direction }

    member self.position = struct (self.row, self.col)

module Direction =
    let vector =
        function
        | N -> Point(-1, 0)
        | E -> Point(0, 1)
        | S -> Point(1, 0)
        | W -> Point(0, -1)

module Grid =
    let rows  (lines: string array) = lines.Length 
    let cols  (lines: string array) = lines[0].Length 
    
    let tryCharAt struct (row, col) (lines: string array) =
        if row >= 0 && row < lines.Length && col >= 0 && col < lines[int row].Length then
            Some(lines[int row][int col])
        else
            None

let outDirections inDirection tile =
    seq {
        match tile, inDirection with
        | '-', (N | S) ->
            yield!
                seq {
                    E
                    W
                }
        | '|', (E | W) ->
            yield!
                seq {
                    N
                    S
                }
        | '/', _ ->
            match inDirection with
            | N -> E
            | E -> N
            | S -> W
            | W -> S
        | '\\', _ ->
            match inDirection with
            | N -> W
            | E -> S
            | S -> E
            | W -> N
        | ('.' | '-' | '|'), _ -> inDirection
        | _ -> failwith "unexpected tile"
    }

let nextCursors grid (cursor: Cursor) =
    let nextPosition = Point.add cursor.position (cursor.direction |> Direction.vector)

    match grid |> Grid.tryCharAt nextPosition with
    | Some tile ->
        let nextDirections = outDirections cursor.direction tile

        nextDirections
        |> Seq.map (fun nextDirection -> Cursor(nextPosition, nextDirection))
    | None -> Seq.empty

let solve grid initialCursor =

    let rec solve (prev: HashSet<Cursor>) cur =
        let next =
            [ let allNext = cur |> Seq.collect (nextCursors grid)

              for n in allNext do
                  if (prev.Add n) then
                      yield n ]

        if (next |> List.isEmpty) then prev else solve prev next

    let allCursors = solve (HashSet()) [ initialCursor ]

    let energizedLocationCount =
        allCursors |> Seq.map _.position |> Seq.distinct |> Seq.length

    energizedLocationCount


let part1 grid = solve grid (Cursor(Point(0, -1), E))

let part2 grid =
    
    let rows = grid |> Grid.rows
    let cols = grid |> Grid.cols
    
    let allStartCursors = [|
        for row in 0..rows-1 do
            yield Cursor((row, -1), E)
            yield Cursor((row, cols), W)
        
        for col in 0..cols-1 do
            yield Cursor((-1, col), S)
            yield Cursor((rows, col), N)
    |]
    
    allStartCursors
    |> Array.Parallel.map (fun cursor -> solve grid cursor)
    |> Array.max
    

let run = runReadAllLines part1 part2

module Tests =
    open Xunit
    open Swensen.Unquote

    let example =
        [| @".|...\...."
           @"|.-.\....."
           @".....|-..."
           @"........|."
           @".........."
           @".........\"
           @"..../.\\.."
           @".-.-/..|.."
           @".|....-|.\"
           @"..//.|...." |]

    [<Fact>]
    let ``part 1 example`` () = part1 example =! 46
    
    [<Fact>]
    let ``part 2 example`` () = part2 example =! 51
