open System.Text.RegularExpressions
// Had to make a seperate module file due to bug in fsi/mono, was core dumping (at least on Ubuntu)
#load "modules/Cyclic.fsx"
open Cyclic


//let input = "9 players; last marble is worth 25 points"
//let input = "10 players; last marble is worth 1618 points"
//let input = "13 players; last marble is worth 7999 points"
//let input = "17 players; last marble is worth 1104 points"
//let input = "30 players; last marble is worth 5807 points"
//let input = "439 players; last marble is worth 71307 points"
let input = "439 players; last marble is worth 7130700 points"

let reg = Regex @"^(\d+) players; last marble is worth (\d+) points$"

let numberOfPlayers = reg.Replace(input, "$1") |> System.Int32.Parse
let lastMarblePoints = reg.Replace(input, "$2") |> System.Int64.Parse

printfn "%i players; last marble is worth %i points" numberOfPlayers lastMarblePoints

type GameState = { NextPlayer: int ; NextMarblePoints: int64; Marbles: Cyclic.Node<int64> ; Collected: (int*int64) list }


let rec play (gameState: GameState) : GameState =
    if gameState.NextMarblePoints > lastMarblePoints then
        gameState
    else
        let newNextPlayer =
            let np = gameState.NextPlayer + 1
            if np < numberOfPlayers then np else 0

        let state =
            if gameState.NextMarblePoints % 23L <> 0L then
                let marbleBeforeNew = gameState.Marbles |> Cyclic.move 1
                let newMarbles = marbleBeforeNew |> Cyclic.insertAfter gameState.NextMarblePoints
                { gameState with Marbles = newMarbles }
            else
                let takenValue, marblesAfterRemove = gameState.Marbles |> Cyclic.move -7 |> Cyclic.remove
                let newMarbles = marblesAfterRemove |> Cyclic.move 1
                let newCollected = (gameState.NextPlayer,takenValue)::(gameState.NextPlayer,gameState.NextMarblePoints)::gameState.Collected
                { gameState with Marbles = newMarbles ; Collected = newCollected }

        play { state with NextPlayer = newNextPlayer ; NextMarblePoints = gameState.NextMarblePoints + 1L }


let initialState = { NextPlayer = 0 ; NextMarblePoints = 1L ; Marbles = Cyclic.singleton 0L ; Collected = [] }

let finishedGame = play initialState

let winner, points =
    finishedGame.Collected
    |> List.groupBy (fst)
    |> List.map (fun (p,scores) -> (p,scores |> List.sumBy (snd)))
    |> List.maxBy (snd)

printfn "Winning points: %i, winning player: %i" points winner
