module Flapjack.InitialiseGame

open FSharp.Reflection
open Flapjack.State
open Flapjack.Operations

let getDiscriminatedUnionCases<'a> () = FSharpType.GetUnionCases typeof<'a>

let generateDeck (): Deck =
    query {
        for value in FSharpType.GetUnionCases typeof<Value> do
            for suit in FSharpType.GetUnionCases typeof<Suit> -> (value, suit)
    }
    |> Seq.map (fun (value, suit) ->
        (FSharpValue.MakeUnion(value, [||]) :?> Value, FSharpValue.MakeUnion(suit, [||]) :?> Suit))
    |> Seq.toList

let initialiseGame () =
    { deck = generateDeck ()
      playerHand = []
      dealerHand = []
      dealerHiddenHand = []
      winner = None }
    |> shuffleDeck
    |> playerDrawCard
    |> dealerDrawCard
    |> playerDrawCard
    |> dealerDrawHidden
