
let input = System.IO.File.ReadAllLines "problem_01a.data"

let values =
    input
    |> Seq.map (fun str -> int (str.Trim()))

let result = values |> Seq.sum

printfn "Result: %d" result