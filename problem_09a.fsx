open System.Text.RegularExpressions

//let input = "9 players; last marble is worth 25 points"
//let input = "10 players; last marble is worth 1618 points"
//let input = "13 players; last marble is worth 7999 points"
//let input = "17 players; last marble is worth 1104 points"
let input = "439 players; last marble is worth 71307 points"

let reg = Regex @"^(\d+) players; last marble is worth (\d+) points$"

let numberOfPlayers = reg.Replace(input, "$1") |> System.Int32.Parse
let lastMarblePoints = reg.Replace(input, "$2") |> System.Int32.Parse

printfn "%i players; last marble is worth %i points" numberOfPlayers lastMarblePoints

type GameState = { Players: Map<int,int list> ; NextPlayer: int ; NextMarblePoints: int; Marbles: int list ; LastPosition: int option }

let insertRight (index: int) (element: 'T) (list: 'T list) =
    let maxIdx = (list |> List.length) - 1
    if index < 0 || index > maxIdx then
        failwithf "idx: %i is out of bounds" index

    if index = maxIdx then
        list @ [ element ]
    else
        let a, b = list |> List.splitAt (index + 1)
        a @ [ element ] @ b

let remove (index: int) (list: 'T list) : 'T*('T list) =
    let maxIdx = (list |> List.length) - 1
    if index < 0 || index > maxIdx then
        failwithf "idx: %i is out of bounds" index

    match list |> List.splitAt index with
    | _, [] ->
        failwithf "Unexpected empty list"
    | a, item::b ->
        (item, a @ b)


let getIndex distance currentIndex marbles =
    let limit = (marbles |> List.length)
    let newIdx = (currentIndex + distance) % limit
    if newIdx < 0 then
        newIdx + limit
    else
        newIdx

let printStatus playerId position marbles =
    let pos =
        match position with
        | None -> -1
        | Some n -> n
    let str =
        marbles
        |> List.mapi (fun i m ->
            if i = pos then
                sprintf "(%2i)" m
            else
                sprintf " %2i " m
            )
        |> List.fold (+) ""

    printfn "[%2i]%s" (playerId + 1) str

let rec play (gameState: GameState) : GameState =
    if gameState.NextMarblePoints > lastMarblePoints then
        gameState
    else
        let newNextPlayer =
            if (gameState.NextPlayer + 1) < (Map.count gameState.Players) then
                gameState.NextPlayer + 1
            else
                0

        let state =
            if gameState.NextMarblePoints = 0 || gameState.NextMarblePoints % 23 <> 0 then
                match gameState.LastPosition with
                | None ->
                    { gameState with Marbles = [ gameState.NextMarblePoints ] ; LastPosition = Some 0 }
                | Some lastPosition ->
                    let index = getIndex 1 lastPosition gameState.Marbles
                    let newMarbles = insertRight index gameState.NextMarblePoints gameState.Marbles
                    { gameState with Marbles = newMarbles ; LastPosition = Some (index+1) }
            else
                match gameState.LastPosition with
                | None ->
                    failwith "Unexpected divisible by 23 with no last position"
                | Some lastPosition ->
                    let indexForRemove = getIndex -7 lastPosition gameState.Marbles
                    let takenMarble, newMarbles = remove indexForRemove gameState.Marbles
                    let newIndex =
                        if indexForRemove >= List.length gameState.Marbles then 0 else indexForRemove

                    let currentPlayerMarbles = gameState.Players |> Map.find gameState.NextPlayer
                    let newPlayerMarbles = takenMarble::gameState.NextMarblePoints::currentPlayerMarbles
                    let newPlayers = gameState.Players |> Map.remove gameState.NextPlayer |> Map.add gameState.NextPlayer newPlayerMarbles

                    { gameState with Players = newPlayers ; Marbles = newMarbles ; LastPosition = Some newIndex }

        //printStatus gameState.NextPlayer state.LastPosition state.Marbles
        play { state with NextPlayer = newNextPlayer ; NextMarblePoints = gameState.NextMarblePoints + 1 }

let initialPlayers = [ 0..numberOfPlayers-1 ] |> List.map (fun n -> n,[]) |> Map.ofList
let initialState = { Players = initialPlayers ; NextPlayer = 0 ; NextMarblePoints = 1 ; Marbles = [ 0 ] ; LastPosition = Some 0 }

let finishedGame = play initialState

let winner, points =
    finishedGame.Players
    |> Map.map (fun p marbles -> (marbles |> List.sum))
    |> Map.toList
    |> List.maxBy (snd)

printfn "Winning points: %i, winning player: %i" points winner
