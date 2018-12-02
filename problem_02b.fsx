
let input = System.IO.File.ReadAllLines "problem_02.data"

let numberOfDifferences a b =
    (a |> Seq.zip b |> Seq.filter (fun (charA,charB) -> charA <> charB) |> Seq.length) + abs ((a |> Seq.length) - (b |> Seq.length))

let rec getCorrect ids =
    match ids with
    | [] -> None
    | [_] -> None
    | head::tail ->
        let maybeSecond = tail |> List.tryFind (fun x -> (numberOfDifferences head x) = 1)
        match maybeSecond with
        | Some x -> Some (head,x)
        | None -> getCorrect tail

let getMachingLetters (a : string) (b : string) : string =
    a |> Seq.zip b |> Seq.filter (fun (charA,charB) -> charA = charB) |> Seq.map (fst) |> Array.ofSeq |> System.String

let result = getCorrect (input |> List.ofArray)

match result with
| None -> printfn "No result found"
| Some (a,b) ->
    printfn "%s, %s" a b
    printfn "Matching letters: %s" <| getMachingLetters a b
