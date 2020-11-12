module Flapjack.DealerTurn

open Flapjack.State
open Flapjack.BlackjackBuilder
open Flapjack.Operations

let dealerTurn state =

    let rec inner (state: State) =
        validate {
            match getValue Dealer state with
            | less when less < getValue Player state ->
                printfn "Dealer hits"

                let! newState = dealerDrawCard state
                return! inner newState
            | _ ->
                printfn "Dealer stands"
                return! state
        }

    inner state
