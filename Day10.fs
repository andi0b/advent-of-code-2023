module aoc23.Day10

open FSharp.Stats

// a pipe is defined by it's entry vectors from both sides, like this: -> PIPE <-
[<Struct>]
type Pipe = Pipe of Vector<float> * Vector<float>

type GridItem =
    | PipeItem of Pipe
    | StartItem
    | Unknown

type Cursor =
    { direction: Vector<float>
      location: Vector<float> }

module Grid =
    let charAt (point: Vector<float>) (lines: string array) =
        match point |> Vector.toArray with
        | [| x; y |] -> lines[int y][int x]
        | _ -> failwith "vector length is not equal to 2"

    let findStart (lines: string array) =
        let y = lines |> Array.findIndex (_.Contains('S'))
        let x = lines[y].IndexOf('S')
        vector [| float x; float y |]


module Pipe =
    let fromChar =
        function
        | '|' -> Pipe(vector [ 0.0; 1.0 ], vector [ 0.0; -1.0 ]) |> PipeItem
        | '-' -> Pipe(vector [ 1.0; 0.0 ], vector [ -1.0; 0.0 ]) |> PipeItem
        | 'J' -> Pipe(vector [ 1.0; 0.0 ], vector [ 0.0; 1.0 ]) |> PipeItem
        | '7' -> Pipe(vector [ 1.0; 0.0 ], vector [ 0.0; -1.0 ]) |> PipeItem
        | 'F' -> Pipe(vector [ -1.0; 0.0 ], vector [ 0.0; -1.0 ]) |> PipeItem
        | 'L' -> Pipe(vector [ -1.0; 0.0 ], vector [ 0.0; 1.0 ]) |> PipeItem
        | 'S' -> StartItem
        | _ -> Unknown

    let isPipe =
        function
        | '|'
        | '-'
        | 'J'
        | '7'
        | 'F'
        | 'L' -> true
        | _ -> false

    let canEnter direction (Pipe(a, b)) = direction = a || direction = b

    let enter cursor =
        function
        | Pipe(inDirection, other)
        | Pipe(other, inDirection) when inDirection = cursor.direction ->
            { direction = other |> Vector.neg
              location = cursor.location + inDirection }
        | _ -> failwith "can't traverse pipe in this direction"

let part1 lines =
    let start = lines |> Grid.findStart

    let possibleDirections =
        [ vector [| 1.0; 0.0 |]
          vector [| -1.0; 0.0 |]
          vector [| 0.0; 1.0 |]
          vector [| 0.0; 1.0 |] ]
        |> List.filter (fun dir ->
            match lines |> Grid.charAt (start + dir) |> Pipe.fromChar with
            | PipeItem pipe -> pipe |> Pipe.canEnter dir
            | _ -> false)

    let cursor =
        { location = start
          direction = possibleDirections[0] }

    let rec loopSize i cursor =
        match lines |> Grid.charAt (cursor.location + cursor.direction) |> Pipe.fromChar with
        | PipeItem nextPipe -> loopSize (i + 1) (Pipe.enter cursor nextPipe)
        | StartItem -> i
        | _ -> failwith "unexpected break in loop"

    (loopSize 1 cursor) / 2


let part2 lines = 0

let run = runReadAllLines part1 part2

module Tests =
    open Xunit
    open Swensen.Unquote

    let example1 =
        [| "-L|F7" //
           "7S-7|"
           "L|7||"
           "-L-J|"
           "L|-JF" |]

    [<Fact>]
    let ``part 1 example 1`` () = part1 example1 =! 4


    let example2 =
        [| "7-F7-" //
           ".FJ|7"
           "SJLL7"
           "|F--J"
           "LJ.LJ" |]

    [<Fact>]
    let ``part 1 example 2`` () = part1 example2 =! 8
