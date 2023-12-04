module aoc23.Day02

open FSharp.Stats
open FSharp.Text.RegexProvider
open FSharp.Text.RegexExtensions
open Xunit
open Swensen.Unquote

type CubeState = vector
type Game = { id: int; rounds: CubeState list }

module Parser =
    type ColorRegex = Regex< @"(?<count>\d*) (?<color>[a-z]*)" >
    type GameRegex = Regex< @"Game (?<gameId>\d*): (?<rounds>.*)" >

    let parseCubeState str =
        let matches = ColorRegex().TypedMatches(str)

        let getCount color =
            matches
            |> Seq.tryFind (fun m -> m.color.Value = color)
            |> Option.map _.count.AsInt
            |> Option.defaultValue 0

        vector [| getCount "red"; getCount "green"; getCount "blue" |]

    let parseRounds str =
        str |> StringEx.splitC ';' |> Seq.map parseCubeState |> Seq.toList

    let parseGame str =
        let match' = GameRegex().TypedMatch(str)

        { id = match'.gameId.AsInt
          rounds = parseRounds match'.rounds.Value }

module Game =
    let cubeCountNeeded game = game.rounds |> Seq.reduce Vector.cptMax

    let canBePlayedWithCubeCount availableCubeCount game =
        (availableCubeCount - cubeCountNeeded game)
        |> Vector.forall (fun diff -> diff >= 0)

module Cube =
    let calcCubePower = Vector.prod

let part1 lines =
    let games = lines |> Seq.map Parser.parseGame
    let availableCubes = vector [| 12; 13; 14 |]

    let possibleGames =
        games |> Seq.filter (Game.canBePlayedWithCubeCount availableCubes)

    possibleGames |> Seq.sumBy _.id


let part2 lines =
    let games = lines |> Seq.map Parser.parseGame
    let gamePowers = games |> Seq.map (Game.cubeCountNeeded >> Cube.calcCubePower)
    gamePowers |> Seq.sum

let run = runReadAllLines part1 part2

module Tests =
    [<Fact>]
    let ``Parse example game 31`` () =

        let parsed =
            "Game 31: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
            |> Parser.parseGame

        let expected =
            { id = 31
              rounds = [ vector [| 20; 8; 6 |]; vector [| 4; 13; 5 |]; vector [| 1; 5; 0 |] ] }

        parsed =! expected

    [<Fact>]
    let ``calculate needed cube count`` () =
        let game =
            "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
            |> Parser.parseGame

        game |> Game.cubeCountNeeded =! vector [| 20; 13; 6 |]

    let example =
        [| "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
           "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
           "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
           "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
           "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green" |]

    [<Fact>]
    let ``Part 1 full example`` () = example |> part1 =! 8

    [<Fact>]
    let ``Part 2 full example`` () = example |> part2 =! 2286
