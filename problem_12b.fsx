open System.Collections.Generic

let input = System.IO.File.ReadAllLines "problem_12.data" |> List.ofArray

let parseState (stateString: string) =
    stateString.Replace("initial state: ", "")
    |> Seq.map (fun c -> c = '#')
    |> Array.ofSeq

let parseRule (ruleString: string) =
    // e.g. ".#.## => #"
    if (String.length ruleString) <> 10 then
        failwithf "Invalid input: %s" ruleString
    let initial = ruleString.[0..4] |> Seq.map (fun c -> c = '#') |> Array.ofSeq
    let final = if ruleString.[9] = '#' then true else false
    (initial, final)

let getValue idx (arr: bool[]) =
    if idx < 0 || idx >= (Array.length arr) then
        false
    else
        arr.[idx]

let findLastIndex f (arr: 'a []) =
    let rec loop n (arr: 'a []) =
        match n with
        | -1 -> raise <| KeyNotFoundException()
        | n ->
            if f arr.[n] then n
            else loop (n-1) arr
    loop (arr.Length - 1) arr

let shrink (state: int64*bool[]) =
    let firstCellNumber, cells = state
    let firstIndex = cells |> Array.findIndex (id) |> int64
    let lastIndex = cells |> findLastIndex (id) |> int64
    if firstIndex = 0L && lastIndex = (int64 (Array.length cells))-1L then
        state
    else
        let newCellNumber = firstCellNumber + firstIndex
        let newCells = Array.sub cells (int firstIndex) (int (lastIndex - firstIndex + 1L))
        (newCellNumber, newCells)


let advance (state: int64*bool[]) rules =
    let firstCellNumber, cells = state
    let newCellNumber = firstCellNumber - 2L
    let newCells =
        Array.init ((Array.length cells)+4) (fun i ->
            let idx = i - 2
            let key =
                [|
                    cells |> getValue (idx-2)
                    cells |> getValue (idx-1)
                    cells |> getValue (idx)
                    cells |> getValue (idx+1)
                    cells |> getValue (idx+2)
                |]

            match rules |> Map.tryFind key with
            | Some true -> true
            | _ -> false
            )
    (newCellNumber, newCells) |> shrink

let getResult (state: int64*seq<bool>) =
    let firstCellNumber, cells = state
    cells
    |> Seq.mapi (fun i value -> ((int64 i)+firstCellNumber,value))
    |> Seq.filter (fun (_,value) -> value)
    |> Seq.map (fst)
    |> Seq.sum

let printState (state: int64*bool[]) (generation: int64) =
    let firstCellNumber, cells = state
    let result = getResult state
    printf "%2i:" generation
    cells
    |> Array.iter (fun cell -> printf "%c" <| if cell then '#' else '.')
    printf " (%i)" firstCellNumber
    printfn " Result: %i" result


let initialState, rules =
    match input with
    | stateString::_::rulesStrings ->
        let state = 0L,(stateString |> parseState)
        let rules = rulesStrings |> List.map (parseRule) |> Map.ofList
        state,rules
    | _ -> failwith "Invalid input"

let numberOfGenerations = 50_000_000_000L
let result =
    let rec loop (state: int64*bool[]) (n: int64) =
        if n > numberOfGenerations then
            getResult state
        else
            let oldCellNumber, oldCells = state
            let newState = advance state rules
            let newCellNumber, newCells = newState
            if newCells = oldCells then
                let newN = n+1L
                printfn "Repeat detected"
                let offset = newCellNumber - oldCellNumber
                let finalCellNumber = newCellNumber + ((numberOfGenerations-newN)*offset)
                let finalState = (finalCellNumber, newCells)
                printfn "n: %i, n+1: %i" n newN
                printfn "offset: %i" offset
                printfn "oldCellNumber: %i, newCellNumber: %i, finalCellNumber: %i" oldCellNumber newCellNumber finalCellNumber
                getResult finalState
            else
                loop newState (n+1L)
    loop initialState 0L

printfn "Result: %i" result
