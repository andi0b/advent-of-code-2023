﻿[<AutoOpen>]
module aoc23.Prelude

open System.IO
open System.Text.RegularExpressions

let tryRegex pattern input =
    let m = Regex.Match(input, pattern)

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None

let (|Regex|_|) pattern input = tryRegex pattern input

let tryRegexG pattern input =
    let m = Regex.Matches(input, pattern)

    if m.Count > 0 then
        Some([ for x in m -> x.Value ])
    else
        None

let (|RegexG|_|) pattern input = tryRegexG pattern input

let tryPrefix (p: string) (s: string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

let (|Prefix|_|) (p: string) (s: string) = tryPrefix p s

module TryParser =
    // convenient, functional TryParse wrappers returning option<'a>
    let tryParseWith (tryParseFunc: string -> bool * _) =
        tryParseFunc
        >> function
            | true, v -> Some v
            | false, _ -> None

    let parseDate = tryParseWith System.DateTime.TryParse
    let parseInt = tryParseWith System.Int32.TryParse
    let parseSingle = tryParseWith System.Single.TryParse
    let parseDouble = tryParseWith System.Double.TryParse
    // etc.

    // active patterns for try-parsing strings
    let (|Date|_|) = parseDate
    let (|Int|_|) = parseInt
    let (|Single|_|) = parseSingle
    let (|Double|_|) = parseDouble

let formatRun part1 part2 = $"Part 1: {part1}, Part 2: {part2}"

let runReadAllLines part1 part2 fileName =
    let lines = File.ReadAllLines(fileName)
    formatRun (lines |> part1) (lines |> part2)

let runReadAllText part1 part2 fileName =
    let lines = File.ReadAllText(fileName)
    formatRun (lines |> part1) (lines |> part2)

let skipPart _ = "skipped"

module StringEx =
    let private asOption =
        function
        | x when x < 0 -> None
        | x -> Some x

    let indexOf (search: string) (input: string) = input.IndexOf(search) |> asOption

    let lastIndexOf (search: string) (input: string) = input.LastIndexOf(search) |> asOption
