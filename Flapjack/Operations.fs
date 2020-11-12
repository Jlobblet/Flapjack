module Flapjack.Operations

open Flapjack.State
open Flapjack.BlackjackBuilder

let drawCard player state =
    blackjack {
        return!
            match (state.deck, player) with
            | [], _ -> state
            | head :: rest, Player ->
                { state with
                      deck = rest
                      playerHand = head :: state.playerHand }
            | head :: rest, Dealer ->
                { state with
                      deck = rest
                      dealerHand = head :: state.dealerHand }
    }

let dealerDrawHidden state =
    blackjack {
        return!
            match state.deck with
            | [] -> state
            | head :: rest ->
                { state with
                      deck = rest
                      dealerHiddenHand = head :: state.dealerHiddenHand }
    }

let playerDrawCard = drawCard Player
let dealerDrawCard = drawCard Dealer

let shuffleDeck state =
    let random = System.Random()

    { state with
          deck = state.deck |> List.sortBy (fun c -> random.Next()) }

let dealerRevealHand state =
    blackjack {
        return!
            { state with
                  dealerHand =
                      List.concat [ state.dealerHand
                                    state.dealerHiddenHand ]
                  dealerHiddenHand = [] }
    }

let determineWinner state =
    match state.winner with
    | Some winner -> winner
    | None ->
        if (getValue Player state) > (getValue Dealer state)
        then Player
        else Dealer
