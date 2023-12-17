module aoc23.Day15

let parse = StringEx.splitC ','

let hash (str: string) =
    (0, str) ||> Seq.fold (fun acc next -> ((acc + (int next)) * 17) % 256)

let part1 str = parse str |> Seq.map hash |> Seq.sum

type Operation =
    | Remove of label: string
    | Add of label: string * focalLength: int

    static member parse =
        function
        | Regex @"([a-z]+)=(\d+)" [ label; focalLength ] -> Add(label, int focalLength)
        | Regex @"([a-z]+)-" [ label ] -> Remove label
        | _ -> failwith ""

let part2 str =

    let lensBoxes = Array.create 256 []

    let mapAt mapper label =
        let boxId = hash label
        lensBoxes[boxId] <- mapper lensBoxes[boxId]

    let removeAt label =
        label
        |> mapAt (fun list ->
            list
            |> List.tryFindIndex (fst >> (=) label)
            |> Option.map (fun index -> list |> List.removeAt index)
            |> Option.defaultValue list)

    let upsertAt label focalLength =
        label
        |> mapAt (fun list ->
            list
            |> List.tryFindIndex (fst >> (=) label)
            |> Option.map (fun index -> list |> List.updateAt index (label, focalLength))
            |> Option.defaultValue (list @ [ (label, focalLength) ]))

    str
    |> parse
    |> Array.map Operation.parse
    |> Seq.iter (function
        | Remove label -> removeAt label
        | Add(label, focalLength) -> upsertAt label focalLength)

    lensBoxes
    |> Array.mapi (fun boxId lenses ->
        lenses
        |> Seq.mapi (fun lensSlot (_, focalLength) -> (boxId + 1) * (lensSlot + 1) * focalLength)
        |> Seq.sum)
    |> Seq.sum

let run = runReadAllText part1 part2

module Tests =
    open Xunit
    open Swensen.Unquote

    let example = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

    [<Fact>]
    let ``part 1 example`` () = part1 example =! 1320

    [<Fact>]
    let ``part 2 example`` () = part2 example =! 145

    [<Fact>]
    let ``parse ops`` () =
        test <@ Operation.parse "rn=1" = Add("rn", 1) @>
        test <@ Operation.parse "ggnd=7" = Add("ggnd", 7) @>
        test <@ Operation.parse "aaab-" = Remove "aaab" @>
