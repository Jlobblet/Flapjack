module Flapjack.State

open System

type Suit =
    | Diamonds
    | Clubs
    | Hearts
    | Spades

type Value =
    | Ace
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King

type Card = Value * Suit

type HandOwner =
    | Player
    | Dealer

let getCardValue: Card -> int =
    function
    | (Ace, _) -> 1
    | (Two, _) -> 2
    | (Three, _) -> 3
    | (Four, _) -> 4
    | (Five, _) -> 5
    | (Six, _) -> 6
    | (Seven, _) -> 7
    | (Eight, _) -> 8
    | (Nine, _) -> 9
    | (Ten, _)
    | (Jack, _)
    | (Queen, _)
    | (King, _) -> 10

let getCardString (card: Card) =
    let getValueString =
        function
        | Ace -> "A"
        | Two -> "2"
        | Three -> "3"
        | Four -> "4"
        | Five -> "5"
        | Six -> "6"
        | Seven -> "7"
        | Eight -> "8"
        | Nine -> "9"
        | Ten -> "10"
        | Jack -> "J"
        | Queen -> "Q"
        | King -> "K"

    let getSuitString =
        function
        | Diamonds -> "♦"
        | Clubs -> "♣"
        | Hearts -> "♥"
        | Spades -> "♠"

    sprintf "%s%s" (getValueString << fst <| card) (getSuitString << snd <| card)


type Deck = Card list
type Hand = Card list

[<StructuredFormatDisplay("{AsString}")>]
type State =
    { deck: Deck
      playerHand: Hand
      dealerHand: Hand
      dealerHiddenHand: Hand
      winner: HandOwner option }
    override this.ToString() =
        sprintf "Your hand: %s (%i)\nDealer's hand: %s %s (%i)\n"
            (this.playerHand
             |> List.map getCardString
             |> String.concat " ") (this._getValue Player)
            (this.dealerHand
             |> List.map getCardString
             |> String.concat " ")
            (this.dealerHiddenHand
             |> List.map (fun _ -> "??")
             |> String.concat " ") (this._getValue Dealer)

    member this.AsString = this.ToString()

    member private this.acesInHand handOwner =
        match handOwner with
        | Player -> this.playerHand
        | Dealer -> this.dealerHand
        |> List.sumBy (function
            | (Ace, _) -> 1
            | _ -> 0)

    member this._getValue handOwner =
        let initialValue =
            match handOwner with
            | Player -> this.playerHand
            | Dealer -> this.dealerHand
            |> List.sumBy getCardValue

        initialValue
        + 10
          * Math.Clamp((21 - initialValue) / 10, 0, this.acesInHand handOwner)
          
let getValue handOwner (state: State) =
    state._getValue handOwner

let lostGame player (state: State): bool = getValue player state > 21

let validateState state =
    if lostGame Player state then { state with winner = Some Dealer }
    else if lostGame Dealer state then { state with winner = Some Player }
    else state
