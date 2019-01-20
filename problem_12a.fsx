open System
let input = System.IO.File.ReadAllLines "problem_12.data" |> List.ofArray

let parseState (stateString: string) =
    stateString.Replace("initial state: ", "")
    |> Seq.mapi (fun i c -> (i,c))
    |> Seq.fold (fun set (i,c) ->
        if c = '#' then
            set |> Set.add i
        else
            set
        ) Set.empty

let parseRule (ruleString: string) =
    // e.g. ".#.## => #"
    if (String.length ruleString) <> 10 then
        failwithf "Invalid input: %s" ruleString
    let initial = ruleString.[0..4] |> Seq.map (fun c -> if c = '#' then true else false) |> Array.ofSeq
    let final = if ruleString.[9] = '#' then true else false
    (initial, final)

let advance state rules =
    if Set.isEmpty state then
        state
    else
        let startIndex = (state |> Set.minElement) - 2
        let endIndex = (state |> Set.maxElement) + 2
        [ startIndex..endIndex ]
        |> List.fold (fun set i ->
            let key =
                [|
                    state |> Set.contains (i-2)
                    state |> Set.contains (i-1)
                    state |> Set.contains (i)
                    state |> Set.contains (i+1)
                    state |> Set.contains (i+2)
                |]
            let newValue =
                match rules |> Map.tryFind key with
                | Some true -> true
                | _ -> false
            if newValue then
                set |> Set.add i
            else
                set
        ) Set.empty

let printState state generation =
    let startIndex = (state |> Set.minElement) - 2
    let endIndex = (state |> Set.maxElement) + 2
    printf "%2i:" generation
    [ startIndex..endIndex ]
    |> List.iter (fun i -> printf "%c" <| if (state |> Set.contains i) then '#' else '.')
    printfn ""


let initialState, rules =
    match input with
    | stateString::_::rulesStrings ->
        let state = stateString |> parseState
        let rules = rulesStrings |> List.map (parseRule) |> Map.ofList
        state,rules
    | _ -> failwith "Invalid input"

printfn "initialState: %A" initialState
let numberOfGenerations = 20
let finalState =
    [ 0 .. numberOfGenerations-1 ]
    |> List.fold (fun state i ->
        printState state i
        advance state rules
        ) initialState
printfn "Final state:"
printState finalState numberOfGenerations
printfn "Result: %i" (finalState |> Seq.sum)