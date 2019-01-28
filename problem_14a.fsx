
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


let numberOfRecipies = 84601
let numberOfRecipiesToKeep = 10

let initialState = { Index1 = 0 ; Index2 = 1 ; Recipies = [| 3 ; 7 |] }
let finalState =
    let rec loop state n =
        if n >= numberOfRecipies && (Array.length state.Recipies) >= (numberOfRecipies + numberOfRecipiesToKeep) then
            state
        else
            loop (advance state) (n+1)
    loop initialState 0

let result =
    finalState.Recipies
    |> Seq.skip numberOfRecipies
    |> Seq.take numberOfRecipiesToKeep
    |> Seq.map (fun value -> value.ToString())
    |> fun s -> (System.String.Join("", s))

printfn "Result: %s" result
