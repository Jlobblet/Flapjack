module Flapjack.Run

open System
open Flapjack.BlackjackBuilder

type Action =
    | PlayAgain
    | Stop
    | Default

let rec runGame () =
    printfn "Dealer hits as long as player has higher value hand"

    let game =
        validate {
            let! initialGame = InitialiseGame.initialiseGame ()
            let! playerTurn = PlayerTurn.playerTurn initialGame
            let! dealerReveal = Operations.dealerRevealHand playerTurn
            let! dealerTurn = DealerTurn.dealerTurn dealerReveal
            return! dealerTurn
        }
        |> Operations.determineWinner

    printfn "%A wins!" game

    let rec getAction () =
        match Console.ReadKey().Key with
        | ConsoleKey.Y -> PlayAgain
        | ConsoleKey.N -> Stop
        | ConsoleKey.Enter -> Default
        | _ -> getAction ()

    printf "Play again? [Y/n] "

    match getAction () with
    | PlayAgain
    | Default ->
        printfn ""
        runGame ()
    | Stop -> ()
