module aoc23.Day01

open System
open Swensen.Unquote
open Xunit

module CalibrationValue =

    let fromDigits (line: string) =
        let chars = line.ToCharArray()
        let firstDigit = chars |> Array.find Char.IsDigit
        let lastDigit = chars |> Array.findBack Char.IsDigit
        $"{firstDigit}{lastDigit}" |> int

    let words =
        [ ("one", 1)
          ("two", 2)
          ("three", 3)
          ("four", 4)
          ("five", 5)
          ("six", 6)
          ("seven", 7)
          ("eight", 8)
          ("nine", 9)
          ("1", 1)
          ("2", 2)
          ("3", 3)
          ("4", 4)
          ("5", 5)
          ("6", 6)
          ("7", 7)
          ("8", 8)
          ("9", 9) ]

    type SearchMode =
        | First
        | Last

    let findValue searchMode line =

        let indexOfFunc, minOrMaxBy =
            match searchMode with
            | First -> (StringEx.indexOf, List.minBy)
            | Last -> (StringEx.lastIndexOf, List.maxBy)

        let findValueWithIndex (searchTerm, value) =
            line
            |> indexOfFunc searchTerm
            |> Option.map (fun index -> {| index = index; value = value |})

        (words |> List.choose findValueWithIndex |> minOrMaxBy _.index).value

    let fromWordsAndDigits (line: string) =
        let first = line |> findValue First
        let last = line |> findValue Last
        first * 10 + last

let part1 = Seq.map CalibrationValue.fromDigits >> Seq.sum
let part2 = Seq.map CalibrationValue.fromWordsAndDigits >> Seq.sum
let run = runReadAllLines part1 part2

module tests =
    let example1 = [| "1abc2"; "pqr3stu8vwx"; "a1b2c3d4e5f"; "treb7uchet" |]

    [<Fact>]
    let ``Part 1 example`` () = part1 example1 =! 142

    let example2 =
        [| "two1nine"
           "eightwothree"
           "abcone2threexyz"
           "xtwone3four"
           "4nineeightseven2"
           "zoneight234"
           "7pqrstsixteen" |]

    [<Fact>]
    let ``Part 2 example`` () = part2 example2 =! 281
