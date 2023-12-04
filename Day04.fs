module aoc23.Day04

open System.Linq
open FSharp.Text.RegexProvider
open FSharp.Text.RegexExtensions
open Xunit
open Swensen.Unquote

type Card =
    { Id: int
      CardNumbers: int list
      WinningNumbers: int list }

module Card =
    type CardRegex = Regex< @"Card +(?<id>\d+): (?<winning>[\d ]*)\|(?<cardNumbers>[\d ]*)" >
    type NumberRegex = Regex< @"(?<num>\d+)" >

    let getAllNumbers str =
        NumberRegex().TypedMatches(str) |> Seq.map _.num.AsInt

    let fromString line =
        let match' = CardRegex().TypedMatch(line)

        { Id = match'.id.AsInt
          CardNumbers = match'.cardNumbers.Value |> getAllNumbers |> Seq.toList
          WinningNumbers = match'.winning.Value |> getAllNumbers |> Seq.toList }

    let matchingNumbers
        { CardNumbers = cn
          WinningNumbers = wn }
        =
        Set.intersect (Set cn) (Set wn)


let part1 lines =
    lines
    |> Seq.map (Card.fromString >> Card.matchingNumbers)
    |> Seq.map (Set.count >> (fun count -> pown 2 (count - 1)))
    |> Seq.sum

let part2 lines =
    let scores =
        lines |> Array.map (Card.fromString >> Card.matchingNumbers >> Set.count)

    let folder (sum, remainingCounts) score =
        match remainingCounts with
        | x :: xs ->
            let xs1, xs2 = xs |> List.splitAt score
            let newXs1 = xs1 |> List.map ((+) x)
            (sum + x, newXs1 @ xs2)
        | _ -> failwith "initial state too short"

    let initialState = (0, List.init scores.Length (fun _ -> 1))
    Seq.fold folder initialState scores |> fst

let run = runReadAllLines part1 part2

module Tests =
    let example =
        [| "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
           "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
           "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
           "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
           "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
           "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11" |]

    [<Fact>]
    let ``part 1`` () = part1 example =! 13

    [<Fact>]
    let ``part 2`` () = part2 example =! 30
