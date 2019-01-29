
type State = { Index1: int ; Index2: int ; Recipies: int[] }

let getLoopedIndex (index: int) (length: int) : int =
    if index < length then
        index
    else
        ((index+1) % length) - 1

let advance state =
    let item1 = state.Recipies.[state.Index1]
    let item2 = state.Recipies.[state.Index2]
    let sum = item1 + item2
    let newDigits =
        if sum <= 9 then
            [| sum |]
        else
            let secondDigit = sum % 10
            let firstDigit = (sum - secondDigit) / 10
            [| firstDigit ; secondDigit |]
    let newRecipies = Array.concat [| state.Recipies ; newDigits |]
    let length = Array.length newRecipies
    let index1 = getLoopedIndex (state.Index1 + 1 + item1) length
    let index2 = getLoopedIndex (state.Index2 + 1 + item2) length
    { Index1 = index1 ; Index2 = index2 ; Recipies = newRecipies }

let printState state =
    state.Recipies
    |> Seq.iteri (fun i value ->
        if i = state.Index1 then
            printf "(%i)" value
        elif i = state.Index2 then
            printf "[%i]" value
        else
            printf " %i " value
        )
    printfn ""

let tryFindScores scores recipies =
    let scoresLength = Array.length scores
    let recipiesLength = Array.length recipies
    if  scoresLength > recipiesLength then
        None
    else
        let idx = recipiesLength - scoresLength
        if (Array.sub recipies idx scoresLength) = scores then
            Some idx
        else
            None
        // let rec loop idx =
        //     if idx+scoresLength > recipiesLength then
        //         None
        //     else
        //         if (Array.sub recipies idx scoresLength) = scores then
        //             Some idx
        //         else
        //             loop (idx + 1)
        // loop 0


printfn "Running..."
//let scoresToFind = [| 9 ; 2 ; 5 ; 1 ; 0 |] 
//let scoresToFind = [| 5 ; 1 ; 5 ; 8 ; 9 |]
//let scoresToFind = [| 5 ; 9 ; 4 ; 1 ; 4 |]
let scoresToFind = [| 0 ; 8 ; 4 ; 6 ; 0; 1 |]

let initialState = { Index1 = 0 ; Index2 = 1 ; Recipies = [| 3 ; 7 |] }
let finalState, numberOfRecipies =
    let rec loop state n =
        match tryFindScores scoresToFind state.Recipies with
        | Some idx -> (state, idx)
        | None ->
            loop (advance state) (n+1)
    loop initialState 2

let result = numberOfRecipies

printfn "Result: %i" result
