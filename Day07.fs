module aoc23.Day07

type Card = Card of char
type Hand = Hand of Card list

module Parser =
    let parseHand = Seq.map Card >> Seq.toList >> Hand

    let parseLine line =
        line
        |> StringEx.splitC ' '
        |> function
            | [| hand; bid |] -> (parseHand hand, bid |> int)
            | _ -> failwith "unexpected input"

module Card =
    let value =
        function
        | Card 'A' -> 14
        | Card 'K' -> 13
        | Card 'Q' -> 12
        | Card 'J' -> 11
        | Card 'T' -> 10
        | Card digit when System.Char.IsDigit digit -> digit |> System.Char.GetNumericValue |> int
        | _ -> failwith "unexpected card"

module Hand =
    let typeScore (Hand cards) =
        cards
        |> List.countBy id
        |> List.map snd
        |> List.sortByDescending id
        |> function
            | [ 5 ] -> 6 // Five of a kind
            | 4 :: _ -> 5 // Four of a kind
            | [ 3; 2 ] -> 4 // Full house
            | 3 :: _ -> 3 // Three of a kind
            | [ 2; 2; 1 ] -> 2 // Two pairs
            | 2 :: _ -> 1 // One pair
            | 1 :: _ -> 0 // High card
            | _ -> failwith "impossible combination"

    /// calculate a comparable score by taking the first-most count and multiply by 10, then adding the second most count.
    /// five of a kind = 50; four of a kind = 41; full house = 32; three of a kind = 31; two pairs = 22; one pair = 21; high card = 11
    let typeScoreJoker (Hand cards) =
        let jokers, others = cards |> List.partition ((=) (Card 'J'))

        others
        |> List.countBy id
        |> List.map snd
        |> List.sortByDescending id
        |> function
            | fst' :: snd' :: _ -> (fst' + jokers.Length) * 10 + snd'
            | _ -> 50 // only one type of card always makes a full house

    /// treat card values as 4 bit ints and shift/add them to form a 20 bit comparable score
    let cardsValueScore (Hand cards) =
        (0, cards) ||> Seq.fold (fun score card -> (score <<< 4) + (card |> Card.value))

    let cardsValueScoreJoker (Hand cards) =
        cards
        |> List.map (function
            | Card 'J' -> Card '0'
            | c -> c)
        |> Hand
        |> cardsValueScore

let solve valueScore typeScore input =
    input
    |> Seq.map Parser.parseLine
    |> Seq.sortBy (fst >> valueScore)
    |> Seq.sortBy (fst >> typeScore)
    |> Seq.mapi (fun i (_, bid) -> bid * (i + 1))
    |> Seq.sum

let part1 = solve Hand.cardsValueScore Hand.typeScore
let part2 = solve Hand.cardsValueScoreJoker Hand.typeScoreJoker

let run = runReadAllLines part1 part2

module Tests =
    open Xunit
    open Swensen.Unquote

    let example = [| "32T3K 765"; "T55J5 684"; "KK677 28"; "KTJJT 220"; "QQQJA 483" |]

    [<Fact>]
    let ``example part 1`` () = part1 example =! 6440

    [<Fact>]
    let ``example part 2`` () = part2 example =! 5905
