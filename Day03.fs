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
                let adjacent =
                    Coordinate.allDirections |> Seq.map (fun dir -> Coordinate.add dir (x, y))

                let adjacentChars = adjacent |> Seq.choose (fun adj -> tryGet adj lines)

                adjacentChars
                |> Seq.exists (fun char -> char <> '.' && not <| System.Char.IsDigit(char)))))
    |> Seq.collect id
    |> Seq.map IndexedDigit.getValue
    |> Seq.sum


let run = runReadAllLines part1 skipPart

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
