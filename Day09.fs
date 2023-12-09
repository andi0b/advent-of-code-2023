module aoc23.Day09

let parseLine = StringEx.splitC ' ' >> Array.map int

let nextValue (numbers: int array) =

    let rec solver line heads =
        let nextLine, allZero =
            (true, line |> List.pairwise)
            ||> List.mapFold (fun allZero (a, b) ->
                let nextValue = (a - b)
                nextValue, allZero && nextValue = 0)

        let nextHeads = (line.Head :: heads)

        match allZero with
        | true -> nextHeads |> List.sum
        | false -> solver nextLine nextHeads

    solver (numbers |> Seq.rev |> Seq.toList) []

let solve lineMapper =
    Array.map parseLine >> Array.map lineMapper >> Seq.map nextValue >> Seq.sum

let part1 = solve id
let part2 = solve Array.rev

let run = runReadAllLines part1 part2

module Tests =
    open Xunit
    open Swensen.Unquote

    let example = [| "0 3 6 9 12 15"; "1 3 6 10 15 21"; "10 13 16 21 30 45" |]

    [<Fact>]
    let ``part 1 example`` () = part1 example =! 114

    [<Fact>]
    let ``part 1 example L1`` () = part1 [| example[0] |] =! 18

    [<Fact>]
    let ``part 1 example L2`` () = part1 [| example[1] |] =! 28

    [<Fact>]
    let ``part 1 example L3`` () = part1 [| example[2] |] =! 68


    [<Fact>]
    let ``part 2 example`` () = part2 example =! 2