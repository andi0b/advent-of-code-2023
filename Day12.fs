module aoc23.Day12

[<Struct>]
type ConditionRecord =
    { offset: int
      states: char list
      groups: int list }

[<Struct>]
type RecordStats =
    { knownOperational: int
      unknownOperational: int
      unknownDamaged: int }

    member x.isSolvable = x.unknownOperational >= 0 && x.unknownDamaged >= 0
    member x.isSolved = x.unknownOperational = 0 && x.knownOperational = 0

module Parser =
    let line str =
        match str |> StringEx.splitC ' ' with
        | [| state; counts |] ->
            { offset = 0
              states = state |> Seq.toList
              groups = counts |> StringEx.splitC ',' |> Seq.map int |> Seq.toList }
        | _ -> failwith "unexpected input"

let (|Operational|Damaged|Unknown|) =
    function
    | '#' -> Operational
    | '.' -> Damaged
    | '?' -> Unknown
    | c -> failwith $"Unknown state '{c}'"

module ConditionRecord =
    let unfold record =
        let repeat times separator list =
            [ for i in 1..times do
                  if i <> 1 then
                      yield! separator |> Option.toList

                  yield! list ]

        { offset = 0
          states = repeat 5 (Some '?') record.states
          groups = repeat 5 None record.groups }

    let offsetTail offset record =
        { offset = record.offset + offset
          states = record.states |> List.skip (min (record.states.Length) offset)
          groups =
            match record.groups with
            | [] -> []
            | _ :: tail -> tail }

    let stats record =
        let totalOperational = record.groups |> List.sum
        let totalDamaged = (record.states |> List.length) - totalOperational

        let counts = record.states |> List.countBy id |> Map
        let knownOperational = (counts |> Map.tryFind '#' |> Option.defaultValue 0)
        let knownDamaged = (counts |> Map.tryFind '.' |> Option.defaultValue 0)

        { knownOperational = knownOperational
          unknownOperational = totalOperational - knownOperational
          unknownDamaged = totalDamaged - knownDamaged }

    let nextGroupSolutionOffsets record =
        let rec allCombinations offset stats group =
            function
            // resolve unknown spring options, recurse without increasing the offset
            | operational, Unknown :: remaining ->
                [ if stats.unknownOperational > 0 then
                      yield!
                          allCombinations
                              offset
                              { stats with
                                  unknownOperational = stats.unknownOperational - 1 }
                              group
                              (operational, '#' :: remaining)

                  if stats.unknownDamaged > 0 then
                      yield!
                          allCombinations
                              offset
                              { stats with
                                  unknownDamaged = stats.unknownDamaged - 1 }
                              group
                              (operational, '.' :: remaining) ]

            // matched a correctly sized group terminated by damaged or at the end
            | operational, ([] | Damaged :: _) when operational = group -> [ offset + 1 ]

            // group became too big, discard
            | operational, _ when operational = group -> []

            // reached the end before finding the group, discard
            | _, [] -> []

            // just continue when not inside an operational group yet
            | 0, Damaged :: remaining -> allCombinations (offset + 1) stats group (0, remaining)

            // hit a damaged spring before reaching needed group size, discard
            | _, Damaged :: _ -> []

            // continue and increment counter when inside an operational group
            | operational, Operational :: remaining ->
                allCombinations (offset + 1) stats group (operational + 1, remaining)

        match (record, stats record) with
        | _, stats when stats.isSolved -> [ 0 ]
        | { groups = nexGroup :: _
            states = states },
          stats when stats.isSolvable -> allCombinations 0 stats nexGroup (0, states)
        | _ -> []

    let allSolutionsCount record =
        ([ (1L, record) ], [ 1 .. record.groups.Length ])
        ||> List.fold (fun records _rank ->
            let expand (multiplier, record) =
                record
                |> nextGroupSolutionOffsets
                |> List.map (fun offsetBy ->
                    {| multiplier = multiplier
                       record = record
                       offsetBy = offsetBy
                       destinationOffset = record.offset + offsetBy |})

            let recordTails =
                records
                |> List.collect expand
                |> List.groupBy _.destinationOffset
                |> List.map (fun (_, x) ->
                    let recordTail = x.Head.record |> offsetTail x.Head.offsetBy
                    let multiplier = x |> List.sumBy (fun i -> i.multiplier)
                    multiplier, recordTail)

            recordTails)
        |> List.sumBy fst

let part1 lines =
    lines
    |> Array.map Parser.line
    |> Array.Parallel.map (ConditionRecord.allSolutionsCount)
    |> Array.sum

let part2 lines =
    lines
    |> Array.map Parser.line
    |> Array.Parallel.map (ConditionRecord.unfold >> ConditionRecord.allSolutionsCount)
    |> Array.sum

let run = runReadAllLines part1 part2

module Tests =
    open Xunit
    open Swensen.Unquote

    [<Theory>]
    [<InlineData("???.### 1,1,3", 1)>]
    [<InlineData(".??..??...?##. 1,1,3", 4)>]
    [<InlineData("?#?#?#?#?#?#?#? 1,3,1,6", 1)>]
    [<InlineData("????.#...#... 4,1,1", 1)>]
    [<InlineData("????.######..#####. 1,6,5", 4)>]
    [<InlineData("?###???????? 3,2,1", 10)>]
    let ``part 1 line-wise`` input expected = part1 [| input |] =! expected


    [<Theory>]
    [<InlineData("???.### 1,1,3", 1)>]
    [<InlineData(".??..??...?##. 1,1,3", 16384)>]
    [<InlineData("?#?#?#?#?#?#?#? 1,3,1,6", 1)>]
    [<InlineData("????.#...#... 4,1,1", 16)>]
    [<InlineData("????.######..#####. 1,6,5", 2500)>]
    [<InlineData("?###???????? 3,2,1", 506250)>]
    let ``part 2 line-wise`` input expected = part2 [| input |] =! expected

    [<Theory>]
    [<InlineData(".# 1", ".#?.#?.#?.#?.# 1,1,1,1,1")>]
    [<InlineData("???.### 1,1,3", "???.###????.###????.###????.###????.### 1,1,3,1,1,3,1,1,3,1,1,3,1,1,3")>]

    let ``unfold`` example expected =
        test <@ (example |> Parser.line |> ConditionRecord.unfold) = (expected |> Parser.line) @>

    [<Fact>]
    let ``next group solution count`` () =
        let record = "???.### 1,3" |> Parser.line
        let solutionOffsets = record |> ConditionRecord.nextGroupSolutionOffsets
        solutionOffsets =! [ 2; 3; 4 ]

    module stats =

        [<Fact>]
        let ``stats`` () =
            let stats = "???.### 1,1,3" |> Parser.line |> ConditionRecord.stats

            stats
            =! { knownOperational = 3
                 unknownDamaged = 1
                 unknownOperational = 2 }

            stats.isSolvable =! true
            stats.isSolved =! false


        [<Fact>]
        let ``stats2`` () =
            let stats = "#.#.### 1,1,3" |> Parser.line |> ConditionRecord.stats

            stats
            =! { knownOperational = 5
                 unknownDamaged = 0
                 unknownOperational = 0 }

            stats.isSolved =! false
            stats.isSolvable =! true

        [<Fact>]
        let ``empty stats`` () =
            let stats = { offset = 0; groups = []; states = [] } |> ConditionRecord.stats

            stats
            =! { knownOperational = 0
                 unknownDamaged = 0
                 unknownOperational = 0 }

            stats.isSolved =! true
            stats.isSolvable =! true
