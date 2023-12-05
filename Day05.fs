module aoc23.Day05

open System.Text.RegularExpressions

type Range = Range of int64 * int64
type CategoryMapping = CategoryMapping of int64 * int64 * int64
type CategoryMap = CategoryMap of CategoryMapping list

type Almanac =
    { Seeds: int64 list
      Maps: CategoryMap list }

module Parser =
    open FSharp.Text.RegexProvider
    open FSharp.Text.RegexExtensions

    type SeedsRegex = Regex< @"seeds: (?<Seeds>[\d ]+)" >
    type CategoryMapRegex = Regex< @"(?<From>[a-z]+)-to-(?<To>[a-z]+) map:\r?\n(?<Mappings>(\d+ \d+ \d+\r?\n?)+)" >
    type CategoryMappingRegex = Regex< @"(?<Destination>\d+) (?<Source>\d+) (?<Length>\d+)" >

    let parseSeeds str =
        str
        |> SeedsRegex().TypedMatch
        |> _.Seeds.Value
        |> StringEx.splitC ' '
        |> Array.map int64
        |> Array.toList

    let parseMap str =
        str
        |> CategoryMapRegex().TypedMatches
        |> Seq.map (fun map ->
            map.Mappings.Value
            |> CategoryMappingRegex(RegexOptions.Multiline).TypedMatches
            |> Seq.map (fun mapping ->
                CategoryMapping(mapping.Destination.AsInt64, mapping.Source.AsInt64, mapping.Length.AsInt64))
            |> Seq.toList)
        |> Seq.map CategoryMap
        |> Seq.toList

    let parseAlmanac str =
        { Seeds = parseSeeds str
          Maps = parseMap str }

module Range =
    let splitAt x =
        function
        | Range(src, _) as r when src >= x -> (None, Some r)
        | Range(src, len) as r when src + len <= x -> (Some r, None)
        | Range(src, len) ->
            let len1 = (x - src) |> max 0L |> min len
            let len2 = len - len1
            (Range(src, len1) |> Some, Range(max x src, len2) |> Some)

    let splitWith (Range(srcOther, lenOther)) (source: Range) =
        let r1, rTemp = source |> splitAt srcOther

        let r2, r3 =
            rTemp
            |> Option.map (splitAt (srcOther + lenOther))
            |> Option.defaultValue (None, None)

        (r1, r2, r3)

module CategoryMapping =
    let inRange x (CategoryMapping(_, src, len)) = x >= src && x < (src + len)
    let map x (CategoryMapping(dest, src, _)) = (x - src) + dest
    let toRange (CategoryMapping(_, src, len)) = Range(src, len)

module CategoryMap =
    let map x (CategoryMap mappings) =
        mappings
        |> List.tryFind (CategoryMapping.inRange x)
        |> Option.map (CategoryMapping.map x)
        |> Option.defaultValue x


let part1 line =
    let { Maps = maps; Seeds = seeds } = line |> Parser.parseAlmanac

    let seedFolder value map = map |> CategoryMap.map value

    seeds |> Seq.map (fun s -> Seq.fold seedFolder s maps) |> Seq.min

let part2 line =
    let { Maps = maps; Seeds = seeds } = line |> Parser.parseAlmanac

    let seedRanges =
        seeds |> List.chunkBySize 2 |> List.map (fun l -> Range(l[0], l[1]))

    let mapRange mapping (Range(src, len)) =
        Range(mapping |> CategoryMapping.map src, len)

    let rec folder mapped unmapped =
        function
        | CategoryMap(mapping :: remainingMappings) ->
            // split unmapped ranges into 1-3 sub-range options, l2s are the ones matching the mapping
            let l1, l2, l3 =
                unmapped
                |> List.map (Range.splitWith (mapping |> CategoryMapping.toRange))
                |> List.unzip3
                |> TupleEx.map3 (List.choose id)

            let l2mapped = l2 |> List.map (mapRange mapping)

            let mapped' = l2mapped @ mapped
            let unmapped' = l1 @ l3
            folder mapped' unmapped' (CategoryMap remainingMappings)

        | CategoryMap [] -> mapped @ unmapped

    maps
    |> Seq.fold (folder []) seedRanges
    |> Seq.map (fun (Range(src, _)) -> src)
    |> Seq.min

let run = runReadAllText part1 part2

module Tests =
    open Xunit
    open Swensen.Unquote

    let example =
        """
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
"""

    [<Fact>]
    let ``Part 1`` () = part1 example =! 35

    [<Fact>]
    let ``Part 2`` () = part2 example =! 46


    [<Fact>]
    let ``Parse Almanac`` () =
        let almanac = example |> Parser.parseAlmanac

        almanac.Seeds =! [ 79; 14; 55; 13 ]
        almanac.Maps.Length =! 7

        almanac.Maps.[0]
        =! CategoryMap [ CategoryMapping(50, 98, 2); CategoryMapping(52, 50, 48) ]

        almanac.Maps.[6]
        =! CategoryMap [ CategoryMapping(60, 56, 37); CategoryMapping(56, 93, 4) ]

    [<Fact>]
    let ``Range.splitAt - inside range`` () =
        Range(5, 10) |> Range.splitAt 10 =! (Range(5, 5) |> Some, Range(10, 5) |> Some)

    [<Fact>]
    let ``Range.splitAt - right of range`` () =
        Range(5, 10) |> Range.splitAt 20 =! (Range(5, 10) |> Some, None)

    [<Fact>]
    let ``Range.splitAt - left of range`` () =
        Range(5, 10) |> Range.splitAt 1 =! (None, Range(5, 10) |> Some)

    [<Fact>]
    let ``Range.splitAt - start point`` () =
        Range(5, 10) |> Range.splitAt 5 =! (None, Range(5, 10) |> Some)

    [<Fact>]
    let ``Range.splitAt - end point`` () =
        Range(5, 10) |> Range.splitAt 15 =! (Range(5, 10) |> Some, None)

    [<Fact>]
    let ``Range.splitWith - completely overlapping`` () =
        //           1    1    2
        // 0....5....0....5....0
        //      <------------->  source
        //           <--->       split with
        //      <-1-><-2-><-3->  result
        Range(5, 15) |> Range.splitWith (Range(10, 5))
        =! (Range(5, 5) |> Some, Range(10, 5) |> Some, Range(15, 5) |> Some)

    [<Fact>]
    let ``Range.splitWith - completely inside`` () =
        Range(10, 5) |> Range.splitWith (Range(5, 15))
        =! (None, Range(10, 5) |> Some, None)

    [<Fact>]
    let ``Range.splitWith - overlapping left`` () =
        Range(10, 10) |> Range.splitWith (Range(5, 10))
        =! (None, Range(10, 5) |> Some, Range(15, 5) |> Some)

    [<Fact>]
    let ``Range.splitWith - no overlap left of other`` () =
        Range(5, 1) |> Range.splitWith (Range(10, 5))
        =! (Range(5, 1) |> Some, None, None)

    [<Fact>]
    let ``Range.splitWith - no overlap right of other`` () =
        Range(20, 1) |> Range.splitWith (Range(10, 5))
        =! (None, None, Range(20, 1) |> Some)
