module aoc23.Day08

open System.Collections

module Parser =
    open FSharp.Text.RegexProvider
    type DirectionsRegex = Regex< @"[LR]+" >
    type NodeRegex = Regex< @"(?<a>[A-Z0-9]+) = \((?<b>[A-Z0-9]+), (?<c>[A-Z0-9]+)\)" >

    let directions text =
        DirectionsRegex().Match(text).Value |> Array.ofSeq

    let nodes text =
        NodeRegex().TypedMatches(text)
        |> Seq.map (fun m -> (m.a.Value, m.b.Value, m.c.Value))


let part1 text =

    let infiniteDirections =
        let directions = text |> Parser.directions
        Seq.initInfinite (fun i -> directions[i % directions.Length])

    let nodeMap =
        text |> Parser.nodes |> Seq.map (fun (a, b, c) -> (a, (b, c))) |> Map.ofSeq

    let nextNodeF =
        function
        | 'L' -> fst
        | 'R' -> snd
        | d -> failwith $"unknown direction {d}"

    let rec findZZZ i (directions: Generic.IEnumerator<char>) =
        function
        | "ZZZ" -> i
        | node ->
            directions.MoveNext() |> ignore
            let nextNode = nodeMap[node] |> (directions.Current |> nextNodeF)
            findZZZ (i + 1) directions nextNode

    findZZZ 0 (infiniteDirections.GetEnumerator()) "AAA"


type BreakCondition =
    | BreakCondition_ZZZ
    | BreakCondition_xxZ

type Solver(text) =

    let directions = text |> Parser.directions

    let directionAt (i: int64) =
        directions[(i % directions.LongLength) |> int]

    let nodeMap =
        text |> Parser.nodes |> Seq.map (fun (a, b, c) -> (a, (b, c))) |> Map.ofSeq

    let sortedNodes =
        nodeMap
        |> Map.keys
        |> Seq.sortByDescending (fun node -> node[^0])
        |> Seq.toArray

    // give each node a number, put **Z to the beginning and **A to the end
    let nodeIdMap = sortedNodes |> Seq.indexed |> Seq.map TupleEx.swap |> Map.ofSeq

    let zCount =
        nodeMap |> Map.keys |> Seq.filter (fun node -> node[^0] = 'Z') |> Seq.length

    let left, right =
        (fst, snd)
        |> TupleEx.map (fun chooser ->
            sortedNodes
            |> Array.map (fun node ->
                let nextNode = nodeMap[node] |> chooser
                nodeIdMap[nextNode]))

    member _.indexOf breakCondition firstNode =

        let mutable state = nodeIdMap |> Map.find firstNode
        let mutable i = 0

        let loopWhile =
            match breakCondition with
            | BreakCondition_ZZZ ->
                let zzzId = nodeIdMap["ZZZ"]
                fun () -> state <> zzzId
            | BreakCondition_xxZ -> fun () -> state > zCount - 1

        while loopWhile () do
            let lookup = if (directionAt i = 'L') then left else right
            state <- lookup[state]
            i <- i + 1

        i

    member _.aNodes = sortedNodes |> Array.filter (fun node -> node[^0] = 'A')

let part1_fast text =
    let solver = Solver text
    solver.indexOf BreakCondition_ZZZ "AAA"

let part2 text =
    let solver = Solver text
    let aNodes = solver.aNodes

    let zNodesAfter =
        aNodes |> Array.map (solver.indexOf BreakCondition_xxZ) |> Array.map int64

    MathNet.Numerics.Euclid.LeastCommonMultiple(zNodesAfter)

let run = runReadAllText part1_fast part2

module Tests =
    open Xunit
    open Swensen.Unquote

    let example =
        """LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
"""

    let example2 =
        """LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
"""

    [<Fact>]
    let ``example part 1`` () = part1 example =! 6

    [<Fact>]
    let ``example part 1 fast`` () = part1_fast example =! 6

    [<Fact>]
    let ``example part 2`` () = part2 example =! 6

    [<Fact>]
    let ``example2 part 2`` () = part2 example2 =! 6
