module aoc23.Day03

open FSharp.Stats
open Xunit
open Swensen.Unquote

type Coordinate = int * int

module Coordinate =
    let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

    let allDirections =
        let s = seq { -1 .. 1 }
        Seq.allPairs s s |> Seq.filter ((<>) (0, 0)) |> Seq.toList

    let adjacent (x, y) =
        allDirections |> Seq.map (fun dir -> add dir (x, y))

let tryGet (x, y) (lines: string array) =
    lines |> Array.tryItem y |> Option.bind (fun line -> line |> Seq.tryItem x)

module IndexedDigit =

    let fromString (str: string) =
        let folder item =
            function
            | [] -> [ [ item ] ]
            | (pos, _) :: _ as ys :: xs when pos - 1 = fst item -> (item :: ys) :: xs
            | list -> [ item ] :: list

        let indexedChars = str |> Seq.indexed |> Seq.filter (snd >> System.Char.IsDigit)

        Seq.foldBack folder indexedChars []

    let getValue digits =
        digits |> Seq.map snd |> Seq.toArray |> System.String |> int


let part1 lines =
    lines
    |> Seq.map IndexedDigit.fromString
    |> Seq.mapi (fun y indexedDigits ->
        indexedDigits
        |> Seq.filter (fun group ->
            group
            |> Seq.exists (fun (x, _) ->
                let adjacentChars =
                    Coordinate.adjacent (x, y) |> Seq.choose (fun adj -> tryGet adj lines)

                adjacentChars
                |> Seq.exists (fun char -> char <> '.' && not <| System.Char.IsDigit(char)))))
    |> Seq.collect id
    |> Seq.map IndexedDigit.getValue
    |> Seq.sum


let part2 lines =

    let anchorCoordinates =
        lines
        |> Seq.mapi (fun y lines ->
            lines
            |> Seq.indexed
            |> Seq.filter (snd >> ((=) '*'))
            |> Seq.map (fun i -> (fst i, y)))
        |> Seq.collect id
        |> Seq.toArray

    let parts =
        lines
        |> Seq.mapi (fun y line ->
            line
            |> IndexedDigit.fromString
            |> Seq.map (fun indexedDigit ->
                (indexedDigit |> IndexedDigit.getValue, indexedDigit |> Seq.map (fun (x, _) -> (x, y)))))
        |> Seq.collect id
        |> Seq.toArray


    let anchorAdjacentParts =
        anchorCoordinates
        |> Seq.map Coordinate.adjacent
        |> Seq.map (fun adjacentCoordinates ->
            parts
            |> Seq.filter (fun (_, points) ->
                points |> Seq.exists (fun point -> adjacentCoordinates |> Seq.contains point))
            |> Seq.map fst
            |> Seq.toArray)
        
    anchorAdjacentParts
    |>Seq.filter (fun parts -> parts.Length = 2)
    |> Seq.map (Seq.reduce (*))
    |> Seq.sum

let run = runReadAllLines part1 part2

module Tests =
    let example =
        [| "467..114.."
           "...*......"
           "..35..633."
           "......#..."
           "617*......"
           ".....+.58."
           "..592....."
           "......755."
           "...$.*...."
           ".664.598.." |]

    [<Fact>]
    let ``Test Part 1`` () = part1 example =! 4361
    
    [<Fact>]
    let ``Test Part 2`` () = part2 example =! 467835
