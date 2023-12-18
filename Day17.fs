module aoc23.Day17

type Grid = int array2d

module Grid =
    let ofLines (lines: string array) =
        lines
        |> Array.map (fun line -> line |> Seq.map (fun char -> System.String([| char |]) |> int))
        |> array2D

    let dimensions grid =
        grid |> Array2D.length1, grid |> Array2D.length2

    let inBounds (row, col) grid =
        row >= 0
        && col >= 0
        && row < (grid |> Array2D.length1)
        && col < (grid |> Array2D.length2)

[<Struct>]
type PossibleDirection =
    | Vertical
    | Horizontal

[<Struct>]
type Cursor =
    { row: int
      col: int
      possibleDirection: PossibleDirection
      heatLoss: int }

module Cursor =
    let possibleMoves minTravel maxTravel grid cursor =

        let next3 increase =
            (Some cursor, seq { 1..maxTravel })
            ||> Seq.scan (fun state _ ->
                state
                |> Option.map increase
                |> Option.filter (fun x -> grid |> Grid.inBounds (x.row, x.col))
                |> Option.map (fun x ->
                    { x with
                        heatLoss = x.heatLoss + grid[x.row, x.col]
                        possibleDirection =
                            match cursor.possibleDirection with
                            | Vertical -> Horizontal
                            | Horizontal -> Vertical }))
            |> Seq.skip minTravel

        seq {
            match cursor.possibleDirection with
            | Horizontal ->
                yield! next3 (fun c -> { c with col = c.col + 1 })
                yield! next3 (fun c -> { c with col = c.col - 1 })
            | Vertical ->
                yield! next3 (fun c -> { c with row = c.row + 1 })
                yield! next3 (fun c -> { c with row = c.row - 1 })
        }
        |> Seq.choose id

    let key cursor =
        struct (cursor.row, cursor.col, cursor.possibleDirection)

let solve minTravel maxTravel lines =

    let grid = Grid.ofLines lines

    let startCursor1 =
        { row = 0
          col = 0
          possibleDirection = Vertical
          heatLoss = 0 }

    let startCursor2 =
        { startCursor1 with
            possibleDirection = Horizontal }

    let rec nextRank previous current =

        let allNext = current |> Seq.collect (Cursor.possibleMoves minTravel maxTravel grid)

        let chosenNext =
            allNext
            |> Seq.groupBy Cursor.key
            |> Seq.map (fun (_g, cursors) -> cursors |> Seq.minBy _.heatLoss)
            |> Seq.filter (fun cur ->
                previous
                |> Map.tryFind (cur |> Cursor.key)
                |> Option.map (fun prevLeastHl -> prevLeastHl >= cur.heatLoss)
                |> Option.defaultValue true)
            |> Seq.toArray

        let updatedPrevious =
            (previous, chosenNext)
            ||> Seq.fold (fun map cur -> map |> Map.add (cur |> Cursor.key) cur.heatLoss)

        if (chosenNext.Length > 0) then
            nextRank updatedPrevious chosenNext
        else
            previous

    let best = nextRank Map.empty [| startCursor1; startCursor2 |]

    let rows, cols = grid |> Grid.dimensions
    let result1 = best |> Map.find (rows - 1, cols - 1, Vertical)
    let result2 = best |> Map.find (rows - 1, cols - 1, Horizontal)

    min result1 result2

let part1 lines =
    solve 1 3 lines
    
let part2 lines =
    solve 4 10 lines    

let run = runReadAllLines part1 part2

module Test =
    open Xunit
    open Swensen.Unquote

    let example =
        [| "2413432311323"
           "3215453535623"
           "3255245654254"
           "3446585845452"
           "4546657867536"
           "1438598798454"
           "4457876987766"
           "3637877979653"
           "4654967986887"
           "4564679986453"
           "1224686865563"
           "2546548887735"
           "4322674655533" |]


    [<Fact>]
    let ``part 1 example`` () = part1 example =! 102
    
    [<Fact>]
    let ``part 2 example`` () = part2 example =! 94
