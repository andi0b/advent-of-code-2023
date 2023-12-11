module aoc23.Day10

open FSharp.Stats

// a pipe is defined by it's entry vectors from both sides, like this: -> PIPE <-
[<Struct>]
type Pipe = Pipe of Vector<float> * Vector<float>

type GridItem =
    | PipeItem of Pipe
    | StartItem
    | EmptyItem
    | UnknownItem

type Cursor =
    { direction: Vector<float>
      position: Vector<float> }

module Grid =
    let findStart (lines: string array) =
        let y = lines |> Array.findIndex (_.Contains('S'))
        let x = lines[y].IndexOf('S')
        vector [| float x; float y |]

    let tryCharAt (point: Vector<float>) (lines: string array) =
        match point |> Vector.raw with
        | [| x; y |] ->
            if y >= 0 && y < lines.Length && x >= 0 && x < lines[int y].Length then
                Some(lines[int y][int x])
            else
                None
        | _ -> failwith "vector length is not equal to 2"

    let tryItemAt point lines =
        lines
        |> tryCharAt point
        |> Option.map (function
            | '|' -> Pipe(vector [ 0.0; 1.0 ], vector [ 0.0; -1.0 ]) |> PipeItem
            | '-' -> Pipe(vector [ 1.0; 0.0 ], vector [ -1.0; 0.0 ]) |> PipeItem
            | 'J' -> Pipe(vector [ 1.0; 0.0 ], vector [ 0.0; 1.0 ]) |> PipeItem
            | '7' -> Pipe(vector [ 1.0; 0.0 ], vector [ 0.0; -1.0 ]) |> PipeItem
            | 'F' -> Pipe(vector [ -1.0; 0.0 ], vector [ 0.0; -1.0 ]) |> PipeItem
            | 'L' -> Pipe(vector [ -1.0; 0.0 ], vector [ 0.0; 1.0 ]) |> PipeItem
            | 'S' -> StartItem
            | '.' -> EmptyItem
            | _ -> UnknownItem)

    let itemAt point lines = tryItemAt point lines |> Option.get
    let charAt point lines = tryCharAt point lines |> Option.get

    let allDirections =
        ((1.0, 0.0), Seq.replicate 4 ())
        ||> Seq.scan (fun (x, y) _ -> (-y, x))
        |> Seq.map (fun (x, y) -> vector [| x; y |])


module Cursor =
    let forward cursor =
        { cursor with
            position = cursor.position + cursor.direction }

    let changeDirection newDirection cursor =
        { cursor with direction = newDirection }

    let leftRightPositions cursor =
        match cursor.direction |> Vector.raw with
        | [| x; y |] -> (vector [| y; -x |], vector [| -y; x |]) |> TupleEx.map ((+) cursor.position)
        | _ -> failwith "vector length is not equal to 2"

    let forwardPosition cursor = cursor.position + cursor.direction

module Pipe =
    let canEnter direction (Pipe(a, b)) = direction = a || direction = b

    let findOutDirection inDirection =
        function
        | Pipe(i, other)
        | Pipe(other, i) when i = inDirection -> other |> Vector.neg
        | _ -> failwith "can't traverse pipe in this direction"

let walkLoop lines =

    let initialLocation = lines |> Grid.findStart

    let initialDirection =
        Grid.allDirections
        |> Seq.find (fun direction ->
            lines
            |> Grid.tryItemAt (initialLocation + direction)
            |> Option.bind (function
                | PipeItem p -> Some p
                | _ -> None)
            |> Option.map (Pipe.canEnter direction)
            |> Option.defaultValue false)

    let initialCursor =
        { position = initialLocation
          direction = initialDirection }

    let nextCursors =
        initialCursor
        |> List.unfold (fun cursor ->
            let nextPosition = cursor |> Cursor.forwardPosition

            match lines |> Grid.itemAt nextPosition with
            | StartItem -> None
            | PipeItem pipe ->
                let nextCursor =
                    { position = nextPosition
                      direction = pipe |> Pipe.findOutDirection cursor.direction }

                Some(nextCursor, nextCursor)

            | _ -> failwith "unexpected break in loop")

    initialCursor :: nextCursors

let part1 lines =
    let allCursors = walkLoop lines
    (allCursors |> List.length) / 2

let part2 lines =

    let allCursors = walkLoop lines

    let loopPositions = allCursors |> Seq.map _.position |> Set.ofSeq

    let rec expand allTiles newLocations =

        let newTiles =
            newLocations
            |> Seq.filter (fun loc ->
                (lines |> Grid.tryCharAt loc).IsSome
                && (loopPositions |> Set.contains loc |> not)
                && (allTiles |> Set.contains loc |> not))
            |> Set.ofSeq

        if (newTiles.Count = 0) then
            allTiles
        else
            expand
                (allTiles |> Set.union newTiles)
                ((Grid.allDirections, newTiles)
                 ||> Seq.allPairs
                 |> Seq.map (fun (direction, newTile) -> newTile + direction))

    let cursorsWithPreviousDirections =
        allCursors
        |> Seq.pairwise
        |> Seq.map (fun (a, b) -> { b with direction = a.direction })
        |> Seq.toList

    let leftPos, rightPos =
        (allCursors @ cursorsWithPreviousDirections)
        |> List.map Cursor.leftRightPositions
        |> List.unzip
        |> TupleEx.map (fun list -> expand Set.empty list)

    lines
    |> Array.iteri (fun y line ->
        line
        |> Seq.iteri (fun x char ->
            let pos = vector [| x; y |]

            let char2 =
                match char with
                | 'J' -> '╯'
                | 'L' -> '╰'
                | '7' -> '╮'
                | 'F' -> '╭'
                | '-' -> '─'
                | '|' -> '│'
                | c -> c

            Spectre.Console.AnsiConsole.Markup(
                if (leftPos |> Set.contains pos) then
                    $"[red]{char2}[/]"
                elif (rightPos |> Set.contains pos) then
                    $"[lime]{char2}[/]"
                elif (loopPositions |> Set.contains pos) then
                    $"[yellow on blue]{char2}[/]"
                else
                    $"[black on pink1]{char2}[/]"
            ))

        printfn "")

    let inner =
        [ leftPos; rightPos ]
        |> Seq.filter (fun set ->
            set
            |> Set.exists (fun vec -> (Vector.get vec 0) = 0.0 || (Vector.get vec 1) = 0.0)
            |> not)
        |> Seq.exactlyOne

    inner.Count

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


    let example3 =
        [| "FF7FSF7F7F7F7F7F---7"
           "L|LJ||||||||||||F--J"
           "FL-7LJLJ||||||LJL-77"
           "F--JF--7||LJLJ7F7FJ-"
           "L---JF-JLJ.||-FJLJJ7"
           "|F|F-JF---7F7-L7L|7|"
           "|FFJF7L7F-JF7|JL---7"
           "7-L-JL7||F7|L7F-7F7|"
           "L.L7LFJ|||||FJL7||LJ"
           "L7JLJL-JLJLJL--JLJ.L" |]

    [<Fact>]
    let ``part 2 example 2`` () = part2 example3 =! 10
