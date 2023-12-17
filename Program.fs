open System.Threading.Tasks
open aoc23

let skip _ = "skipped"
let days =
    [ Day01.run
      Day02.run
      Day03.run
      Day04.run
      Day05.run
      Day06.run
      Day07.run
      Day08.run
      Day09.run
      Day10.run
      Day11.run
      Day12.run
      Day13.run
      Day14.run
      Day15.run ]
    |> List.mapi (fun i run -> fun () -> run $"inputs/day%02u{i + 1}.txt")
let runAll () =
    let tasks =
        days
        |> List.indexed
        |> List.rev
        |> List.map (fun (i, r) ->
            task {
                let! result = Task.Run r
                return (i, result)
            })

    task {
        for task in tasks do
            let! i, result = task
            printfn $"Day {i + 1} {result}"
    }
    |> Task.WaitAll

open TryParser

[<EntryPoint>]
let Main args =
    match args with
    | [| Int day |] when day > 0 && day <= 25 ->

        match days |> List.tryItem (day - 1) with
        | Some implementation ->
            printfn $"Running day {day}:"
            printfn $"{implementation ()}"
            0

        | None ->
            printfn $"Could not find an implementation for day {day}"
            1

    | [| "latest" |] when days.Length > 0 ->
        printfn $"Running latest (day {days.Length}):"
        printf $"{(days |> List.last) ()}"
        0

    | [||] ->
        printfn $"Running all days (1 - {days.Length})"
        runAll ()
        0

    | _ ->
        printf "Expect either a number, 'latest', no parameters"
        1
