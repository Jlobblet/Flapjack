module Flapjack.PlayerTurn

open System
open Flapjack.State
open Flapjack.Operations
open Flapjack.BlackjackBuilder

type playerAction =
    | Fold
    | Stand
    | Hit

let getAction () =
    let rec inner () =
        match Console.ReadKey().KeyChar |> Char.ToUpper with
        | 'F' -> Fold
        | 'S' -> Stand
        | 'H' -> Hit
        | _ -> inner ()

    printf "[F]old, [S]tand, or [H]it? "
    let r = inner ()
    printfn ""
    r

let playerTurn state =
    let rec inner state =
        validate {
            match getAction () with
            | Fold ->
                printfn "You folded!"
                return! { state with winner = Some Dealer }
            | Stand ->
                printfn "Stand"
                return! state
            | Hit ->
                printfn "Hit!"

                let! newState = playerDrawCard state
                return! inner newState
        }

    inner state
