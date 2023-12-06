module aoc23.Day06

[<Measure>]
type mm

[<Measure>]
type ms

type Game = { Time: int64<ms>; Distance: int64<mm> }

module Parser =
    open FSharp.Text.RegexProvider
    open FSharp.Text.RegexExtensions

    type NumberRegex = Regex< @"(\d+)" >

    let parse lines =
        let parsedNumbers =
            lines
            |> Array.map (fun line -> NumberRegex().TypedMatches(line) |> Seq.map _.AsInt64)

        match parsedNumbers with
        | [| times; distances |] ->
            Seq.zip times distances
            |> Seq.map (fun (t, d) ->
                { Time = t * 1L<ms>
                  Distance = d * 1L<mm> })
            |> Seq.toList
        | _ -> failwith "Invalid input"

    let parseAsOne (lines: string array) =
        let parseNum line =
            line |> StringEx.splitC ':' |> Seq.last |> StringEx.replace " " "" |> int64

        { Time = (parseNum lines[0]) * 1L<ms>
          Distance = (parseNum lines[1]) * 1L<mm> }

module Game =

    let speedIncrease = 1L<mm / ms / ms>

    let simulate buttonPressTime game =
        let timeLeft = game.Time - buttonPressTime
        let speed = buttonPressTime * speedIncrease
        speed * timeLeft

    let possibleButtonPressTimes game = seq { 0L<ms> .. 1L<ms> .. game.Time }

    let recordBeatingGameCount game =
        game
        |> possibleButtonPressTimes
        |> Seq.map (fun time -> simulate time game)
        |> Seq.filter (fun distance -> distance > game.Distance)
        |> Seq.length

    // Solved game equation d=(t-b)*b (distance=(time-buttonpresses)*buttonpresses with sympy to this:
    // [t/2 - sqrt(-4*d + t**2)/2, t/2 + sqrt(-4*d + t**2)/2]        
    let algorithmicRecordBeatingGameCount { Distance = distance; Time = time } =
        
        let solve (d: float) (t: float) =
            let min, max =
                ((-), (+))
                |> TupleEx.map (fun f -> f (t / 2.0) (sqrt (-4.0 * d + t ** 2.0) / 2.0))

            ((ceil max) - (floor min) - 1.0) |> int64

        solve (float distance) (float time)

let part1 lines =
    let games = lines |> Parser.parse
    games |> Seq.map Game.algorithmicRecordBeatingGameCount |> Seq.reduce (*)

let part2 lines =
    let game = lines |> Parser.parseAsOne
    Game.algorithmicRecordBeatingGameCount game

let run = runReadAllLines part1 part2


module Tests =
    open Xunit
    open Swensen.Unquote

    let example = [| "Time:      7  15   30"; "Distance:  9  40  200" |]

    [<Fact>]
    let ``Parse example`` () =
        Parser.parse example
        =! [ { Time = 7L<ms>; Distance = 9L<mm> }
             { Time = 15L<ms>; Distance = 40L<mm> }
             { Time = 30L<ms>; Distance = 200L<mm> } ]

    [<Fact>]
    let ``Simulate example`` () =
        let game = { Time = 7L<ms>; Distance = 9L<mm> }
        game |> Game.simulate 0L<ms> =! 0L<mm>
        game |> Game.simulate 2L<ms> =! 10L<mm>
        game |> Game.simulate 7L<ms> =! 0L<mm>

    [<Fact>]
    let ``Part 1`` () = part1 example =! 288

    [<Fact>]
    let ``Part 2`` () = part2 example =! 71503
