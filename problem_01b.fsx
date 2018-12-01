
let input = System.IO.File.ReadAllLines "problem_01a.data"

let inputValues =
    input
    |> Array.map (fun str -> int64 (str.Trim()))

let rec getResult frequenciesSeen currentFreq i =
    let change = inputValues.[i]
    let newFreq = currentFreq + change
    if frequenciesSeen |> Set.contains newFreq then    
        newFreq
    else
        let newI = if i+1 = (inputValues |> Array.length) then 0 else i + 1
        getResult (frequenciesSeen |> Set.add newFreq) newFreq newI

let result = getResult ([ 0L ] |> Set.ofList) 0L 0

printfn "Result: %d" result
