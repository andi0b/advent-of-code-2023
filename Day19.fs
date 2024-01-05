module aoc23.Day19

type Comparer =
    | GreaterThan of property: char * gt: int
    | LowerThan of property: char * lt: int
    | Any

[<Struct>]
type Rule = Rule of comparer: Comparer * resolution: string

type Part = Map<char, int>
type Workflows = Map<string, Rule array>

module Parser =
    open FSharp.Text.RegexProvider
    open FSharp.Text.RegexExtensions

    type PartRegex = Regex< @"^{(?<properties>.*)}" >
    type PartPropertyRegex = Regex< @"((?<name>[a-z]?)=(?<value>\d+))" >

    let parts =
        PartRegex(System.Text.RegularExpressions.RegexOptions.Multiline).TypedMatches
        >> Seq.map _.properties.Value
        >> Seq.map (
            PartPropertyRegex().TypedMatches
            >> Seq.map (fun m -> m.name.AsChar, m.value.AsInt)
            >> Part
        )
        >> Seq.toArray

    type RuleRegex = Regex< @"(?<property>[a-z])(?<comparison>[<>])(?<value>\d+):(?<resolution>\w+)" >

    let private rules input =
        [| for m in RuleRegex().TypedMatches(input) do
               yield
                   Rule(
                       ((m.property.AsChar, m.value.AsInt)
                        |> match m.comparison.AsChar with
                           | '>' -> GreaterThan
                           | '<' -> LowerThan
                           | _ -> failwith ""),
                       m.resolution.Value
                   )

           yield Rule(Any, input |> StringEx.splitC ',' |> Array.last) |]

    type WorkflowRegex = Regex< @"(?<name>[a-z]+){(?<rules>.*)}" >

    let workflows =
        WorkflowRegex().TypedMatches
        >> Seq.map (fun m -> m.name.Value, m.rules.Value |> rules)
        >> Map.ofSeq

module Part =
    let score (part: Part) = part |> Seq.sumBy _.Value

module Rule =
    let isMatch (part: Part) =
        function
        | Rule(Any, _) -> true
        | Rule(GreaterThan(prop, value), _) -> part[prop] > value
        | Rule(LowerThan(prop, value), _) -> part[prop] < value

module Workflow =
    let (|Accepted|Rejected|Workflow|) =
        function
        | "A" -> Accepted
        | "R" -> Rejected
        | x -> Workflow x

    let findPartResolution part (workflows: Workflows) =
        let rec traverse rule =
            rule
            |> Array.find (Rule.isMatch part)
            |> function
                | Rule(_, Workflow next) -> traverse workflows[next]
                | Rule(_, resolution) -> resolution

        traverse workflows["in"]

let part1 input =
    let workflows = input |> Parser.workflows

    input
    |> Parser.parts
    |> Array.filter (fun part ->
        workflows
        |> Workflow.findPartResolution part
        |> function
            | Workflow.Accepted -> true
            | _ -> false)
    |> Array.sumBy Part.score


type PartRange = Map<char, int * int>

module PartRange =
    let splitAt prop at (pr: PartRange) =

        let from, to' = pr[prop]

        let lower = from, min to' (at-1)
        let upper = max from at, to'

        (lower, upper)
        |> TupleEx.map (fun (f, t) -> if t - f > 0 then Some(f,t) else None)
        |> TupleEx.map (Option.map (fun range -> pr |> Map.add prop range))

    let count (pr: PartRange) =
        (1L, pr)
        ||> Map.fold (fun acc _ (f, t) -> acc * int64 (t - f + 1)) 


let part2 input =
    let workflows = input |> Parser.workflows
   
    let nextResolutions (workflowName, partRange) =
       
        let rec resolve resolutions partRange =
            function
            | [ Rule(Any, next) ] -> (next, partRange) :: resolutions
            |  rule :: remainingRules  ->
                
                let next, resolution, remaining = 
                    match rule with
                    | Rule(GreaterThan(prop, value), next) ->
                         let lower, upper = partRange |> PartRange.splitAt prop (value + 1)
                         next, upper, lower
                    | Rule(LowerThan(prop, value), next) ->
                        let lower, upper = partRange |> PartRange.splitAt prop value
                        next, lower, upper
                    | _ -> failwith "unexpected rules"
                
                let nextResolutions =                     
                    resolution
                    |> Option.map (fun r -> (next,r) :: resolutions)
                    |> Option.defaultValue resolutions
                    
                match remaining with
                | Some r -> resolve nextResolutions r remainingRules
                | None -> nextResolutions
                    
            | _ -> failwith "unexpected rules"
        
        
        resolve [] partRange (workflows[workflowName] |> List.ofArray)
           
    
    let rec countAccepted acc partRanges =
        
        let resolutions =
            partRanges
            |> List.collect nextResolutions
        
        let acceptedCount =
            resolutions
            |> Seq.choose (fun (workflowName, partRange) ->
                match workflowName with
                | Workflow.Accepted -> Some(partRange |> PartRange.count)
                | _ -> None)
            |> Seq.sum
        
        let unresolved =
            resolutions
            |> List.filter(fun (workflowName,_) ->
                           match workflowName with
                           | Workflow.Accepted | Workflow.Rejected -> false
                           | _ -> true)
            
        match unresolved with
        | [] -> acc + acceptedCount
        | _ -> countAccepted (acc + acceptedCount) unresolved
        
    
    let start =
        "in", Map([ 'x', (1, 4000); 'm', (1, 4000); 'a', (1, 4000); 's', (1, 4000) ])    
    
    countAccepted 0L [start]


let run = runReadAllText part1 part2

module Tests =

    open Xunit
    open Swensen.Unquote

    let example =
        """px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}
"""

    [<Fact>]
    let ``part 1 example`` () = part1 example =! 19114

    [<Fact>]
    let ``part 2 example`` () = part2 example =! 167409079868000L

    [<Fact>]
    let ``parsing example workflows`` () =
        let workflows = Parser.workflows example

        workflows.Count =! 11

        workflows["px"]
        =! [| Rule(LowerThan('a', 2006), "qkq")
              Rule(GreaterThan('m', 2090), "A")
              Rule(Any, "rfg") |]


    [<Fact>]
    let ``parsing example parts`` () =

        let parts = Parser.parts example

        parts.Length =! 5

        parts.[0]
        =! Map(
            seq {
                'x', 787
                'm', 2655
                'a', 1222
                's', 2876
            }
        )


    [<Fact>]
    let ``split part range`` () =
        Map([ 'x', (1, 4000); 'y', (1, 4000) ]) |> PartRange.splitAt 'x' 2000
        =! (Some(Map([ 'x', (1, 1999); 'y', (1, 4000) ])), Some(Map([ 'x', (2000, 4000); 'y', (1, 4000) ])))


    [<Fact>]
    let ``split part range 2`` () =
        Map([ 'x', (1, 4000); 'y', (1, 4000) ]) |> PartRange.splitAt 'x' 6000
        =! (Some(Map([ 'x', (1, 4000); 'y', (1, 4000) ])), None)
