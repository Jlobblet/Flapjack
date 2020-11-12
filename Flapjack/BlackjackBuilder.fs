module Flapjack.BlackjackBuilder

open Flapjack.State

type BlackjackBuilder() =
    member _.Bind(state, f) =
        let validatedState = validateState state
        printfn "%A" validatedState
        f validatedState

    member _.Return(state) = state

    member this.ReturnFrom(state) = this.Bind(state, (fun s -> s))

type ValidatorBuilder() =
    member _.Bind(state, f) =
        match state.winner with
        | None -> f state
        | Some _ -> state

    member _.Return(state) = state

    member this.ReturnFrom(state) = this.Bind(state, (fun s -> s))

let blackjack = BlackjackBuilder()

let validate = ValidatorBuilder()
