module aoc23.Day15

let parse = StringEx.splitC ','

let hash (str: string) =
    (0, str) ||> Seq.fold (fun acc next -> ((acc + (int next)) * 17) % 256)

let part1 str = parse str |> Seq.map hash |> Seq.sum


type LensBoxes() =
    let lensBoxes = Array.create 256 List<(string * int)>.Empty

    let mapAt mapper label =
        let boxId = hash label
        lensBoxes[boxId] <- mapper lensBoxes[boxId]

    member x.removeAt label =
        label
        |> mapAt (fun list ->
            list
            |> List.tryFindIndex (fst >> (=) label)
            |> Option.map (fun index -> list |> List.removeAt index)
            |> Option.defaultValue list)

    member x.upsertAt label focalLength =
        label
        |> mapAt (fun list ->
            list
            |> List.tryFindIndex (fst >> (=) label)
            |> Option.map (fun index -> list |> List.updateAt index (label, focalLength))
            |> Option.defaultValue (list @ [ (label, focalLength) ]))

    member x.score =
        lensBoxes
        |> Array.mapi (fun boxId lenses ->
            lenses
            |> Seq.mapi (fun lensSlot  (_, focalLength) -> (boxId + 1) * (lensSlot + 1) * focalLength)
            |> Seq.sum)
        |> Seq.sum

let part2 str =
    let lensBoxes = LensBoxes()

    str
    |> parse
    |> Seq.iter (function
        | Regex @"([a-z]+)=(\d+)" [ label; focalLength ] -> lensBoxes.upsertAt label (int focalLength)
        | Regex @"([a-z]+)-" [ label ] -> lensBoxes.removeAt label
        | _ -> failwith "unknown operation")

    lensBoxes.score

let run = runReadAllText part1 part2

module Tests =
    open Xunit
    open Swensen.Unquote

    let example = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

    [<Fact>]
    let ``part 1 example`` () = part1 example =! 1320

    [<Fact>]
    let ``part 2 example`` () = part2 example =! 145
