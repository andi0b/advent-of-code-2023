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
        allDirections |> Seq.map (fun dir -> add dir (x, y)) |> Set.ofSeq

let tryGet (x, y) (lines: string array) =
    lines |> Array.tryItem y |> Option.bind (fun line -> line |> Seq.tryItem x)

type Part =
    { Number: int; Points: (int * int) Set }

module Part =

    let private getValue digits =
        digits |> Seq.map snd |> Seq.toArray |> System.String |> int

    let fromLine lineId line =
        let folder item =
            function
            | [] -> [ [ item ] ]
            | (pos, _) :: _ as ys :: xs when pos - 1 = fst item -> (item :: ys) :: xs
            | list -> [ item ] :: list

        let indexedDigits = line |> Seq.indexed |> Seq.filter (snd >> System.Char.IsDigit)

        Seq.foldBack folder indexedDigits []
        |> Seq.map (fun digits ->
            { Number = digits |> Seq.map snd |> Seq.toArray |> System.String |> int
              Points = digits |> Seq.map fst |> Seq.map (fun x -> (x, lineId)) |> Set.ofSeq })

    let fromLines lines =
        lines |> Seq.mapi fromLine |> Seq.collect id


let part1 lines =
    let parts = lines |> Part.fromLines

    let partsNextToSymbols =
        parts
        |> Seq.filter (fun part ->
            part.Points
            |> Seq.exists (fun point ->
                let adjacentChars =
                    Coordinate.adjacent point |> Seq.choose (fun adj -> tryGet adj lines)

                adjacentChars
                |> Seq.exists (fun char -> char <> '.' && not <| System.Char.IsDigit(char))))

    partsNextToSymbols |> Seq.sumBy _.Number


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

    let parts = lines |> Part.fromLines |> Seq.toArray

    let anchorAdjacentParts =
        anchorCoordinates
        |> Seq.map Coordinate.adjacent
        |> Seq.map (fun adjacentCoordinates ->
            parts
            |> Array.filter (fun part -> Set.intersect part.Points adjacentCoordinates |> Set.count > 0))

    anchorAdjacentParts
    |> Seq.filter (fun parts -> parts.Length = 2)
    |> Seq.map (Seq.map _.Number >> Seq.reduce (*))
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
