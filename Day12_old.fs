module aoc23.Day12_old

[<Struct>]
type ConditionRecord = { offset: int;  states: char list; groups: int list }

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

let counts states =
    let rec readOneGroup =
        function
        | 0, Damaged :: remaining -> readOneGroup (0, remaining)
        | count, [ Operational ] -> (count + 1, [])
        | count, Operational :: remaining -> readOneGroup (count + 1, remaining)
        | count, Damaged :: remaining -> (count, remaining)
        | s -> s

    let rec allGroups states =
        seq {
            let count, remaining = readOneGroup (0, states)

            if (count > 0) then
                yield count
                yield! allGroups remaining
        }

    allGroups states |> Seq.toList

[<Struct>]
type RecordStats =
    { isSolvable: bool
      unknownOperational: int
      unknownDamaged: int }

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

    
    let stats record =
        let operational = record.groups |> List.sum
        let damaged = (record.states |> List.length) - operational

        let counts = record.states |> List.countBy id |> Map

        let unknownOperational = operational - (counts|> Map.tryFind '#' |> Option.defaultValue 0)
        let unknownDamaged = damaged - (counts |> Map.tryFind '.' |> Option.defaultValue 0)

        { isSolvable = unknownOperational > 0 && unknownDamaged > 0 
          unknownOperational = unknownOperational
          unknownDamaged = unknownDamaged }
    

    let nextGroupSolutions =
        function
        | { groups = group::remainingGroups } as record -> [record]
            
            
        
        | record -> [ record ]
            
        


    let possibleCombinations record =
        // finding combinations in reverse for efficiency
        let next state combinations =
            match state with
            | Unknown ->
                seq {
                    for s in combinations do
                        yield '#' :: s
                        yield '.' :: s
                }
            | other -> combinations |> Seq.map (fun s -> other :: s)

        let revCounts = record.groups |> Seq.rev |> Seq.toArray
        let neededOperational = record.groups |> Seq.sum
        let neededDamaged = record.states.Length - neededOperational

        let rec find combinations =
            function
            | [] -> combinations |> List.filter (counts >> (=) record.groups)
            | state :: remaining ->

                let remainingPossibleOperational =
                    remaining
                    |> Seq.filter (function
                        | Unknown
                        | Operational -> true
                        | _ -> false)
                    |> Seq.length

                let remainingPossibleDamaged =
                    remaining
                    |> Seq.filter (function
                        | Unknown
                        | Damaged -> true
                        | _ -> false)
                    |> Seq.length

                let possibleNextCombinations =
                    combinations
                    |> next state
                    |> Seq.filter (fun combination ->
                        let combinationOperational =
                            combination
                            |> Seq.filter (function
                                | Operational -> true
                                | _ -> false)
                            |> Seq.length

                        let combinationDamaged =
                            combination
                            |> Seq.filter (function
                                | Damaged -> true
                                | _ -> false)
                            |> Seq.length

                        combinationOperational + remainingPossibleOperational >= neededOperational
                        && combinationDamaged + remainingPossibleDamaged >= neededDamaged)
                    |> Seq.filter (
                        counts
                        >> function
                            | cur :: tail ->
                                cur <= (revCounts |> Array.tryItem tail.Length |> Option.defaultValue 0)
                                && Seq.forall2 (=) (tail |> List.rev) revCounts

                            | _ -> true
                    )
                    |> Seq.toList

                find possibleNextCombinations remaining

        find [ [] ] (record.states |> List.rev) |> List.length

let part1 lines =
    lines
    |> Array.map Parser.line
    |> Array.Parallel.map (ConditionRecord.possibleCombinations)
    |> Array.sum

let part2 lines =
    lines
    |> Array.map Parser.line
    |> Array.Parallel.map (ConditionRecord.unfold >> ConditionRecord.possibleCombinations)
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
    let ``stats`` () =
        let stats = "???.### 1,1,3" |> Parser.line |> ConditionRecord.stats

        stats
        =! { isSolvable = true
             unknownDamaged = 1
             unknownOperational = 2 }