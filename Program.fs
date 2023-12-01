open System.Threading.Tasks
open aoc23

let skip = (fun () -> "skipped")

let days = [ Day01.run ]


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
            printf $"Day {day} {implementation ()}"
            0

        | None ->
            printf $"Could not find an implementation for day {day}"
            1

    | [| "latest" |] when days.Length > 0 ->
        printf $"Day {days.Length} {(days |> List.last) ()}"
        0

    | [||] ->
        runAll ()
        0

    | _ ->
        printf "Expect either a number no parameters"
        1
