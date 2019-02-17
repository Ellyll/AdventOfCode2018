
type State = { Index1: int ; Index2: int ; Recipies: System.Collections.Generic.List<int> }

let newScores a b =
    match a+b with
    | 0 -> [| 0 |]
    | 1 -> [| 1 |]
    | 2 -> [| 2 |]
    | 3 -> [| 3 |]
    | 4 -> [| 4 |]
    | 5 -> [| 5 |]
    | 6 -> [| 6 |]
    | 7 -> [| 7 |]
    | 8 -> [| 8 |]
    | 9 -> [| 9 |]
    | 10 -> [| 1 ; 0 |]
    | 11 -> [| 1 ; 1 |]
    | 12 -> [| 1 ; 2 |]
    | 13 -> [| 1 ; 3 |]
    | 14 -> [| 1 ; 4 |]
    | 15 -> [| 1 ; 5 |]
    | 16 -> [| 1 ; 6 |]
    | 17 -> [| 1 ; 7 |]
    | 18 -> [| 1 ; 8 |]
    | n -> failwithf "Invalid result: %i" n


let advance state =
    let a = state.Recipies.[state.Index1]
    let b = state.Recipies.[state.Index2]
    let scores = newScores  a b
    state.Recipies.AddRange scores
    let length = state.Recipies.Count
    let index1 = (state.Index1 + 1 + a) % length
    let index2 = (state.Index2 + 1 + b) % length
    { Index1 = index1 ; Index2 = index2 ; Recipies = state.Recipies }


let printState state =
    state.Recipies
    |> Seq.iteri (fun i value ->
        if i = state.Index1 then
            printf "[%i]" value
        elif i = state.Index2 then
            printf "(%i)" value
        else
            printf " %i " value
        )
    printf " --- I1=%i I2=%i" state.Index1 state.Index2
    printfn ""

let existsAt (subList: System.Collections.Generic.List<int>) (index: int) (parentList: System.Collections.Generic.List<int>) =
    let subLength = subList.Count
    let parentLength = parentList.Count
    if subLength > parentLength || index < 0 || index > parentLength-subLength then
        false
    else
        let extract =
            [| index .. (index+subLength-1) |]
            |> Array.map (fun idx -> parentList.[idx])
        extract = (subList |> Array.ofSeq)


let tryFindScores (scores: System.Collections.Generic.List<int>) (recipies: System.Collections.Generic.List<int>) =
    let scoresLength = Seq.length scores
    let recipiesLength = Seq.length recipies
    if existsAt scores (recipiesLength - scoresLength) recipies then
        Some (recipiesLength - scoresLength)
    elif existsAt scores (recipiesLength - scoresLength - 1) recipies then
        Some (recipiesLength - scoresLength - 1)
    else
        None        

printfn "Running..."
//let input = [ 5 ; 1 ; 5 ; 8 ; 9 ] // should be 9
//let input = [ 0 ; 1 ; 2 ; 4 ; 5 ] // should be 5
//let input = [ 9 ; 2 ; 5 ; 1 ; 0 ] // should be 18
//let input = [ 5 ; 9 ; 4 ; 1 ; 4 ] // should be 2018
let input = [ 0 ; 8 ; 4 ; 6 ; 0; 1 ]

let scoresToFind = System.Collections.Generic.List<int>(input)

let initialState = { Index1 = 1 ; Index2 = 0 ; Recipies = System.Collections.Generic.List<int>([| 3 ; 7 |]) }

let stopwatch = System.Diagnostics.Stopwatch()
stopwatch.Start()

let finalState, scoresIndex =
    let rec loop state n =
        match tryFindScores scoresToFind state.Recipies with
        | Some idx -> (state, idx)
        | None ->
            loop (advance state) (n+1)
    loop initialState 2

stopwatch.Stop()

let result = scoresIndex

printfn "Result: %i" result
printfn "Elapsed: %ims" stopwatch.ElapsedMilliseconds
