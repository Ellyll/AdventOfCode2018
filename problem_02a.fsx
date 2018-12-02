
let input = System.IO.File.ReadAllLines "problem_02.data"

let countLetters (str : string) =
    str |> Seq.fold (fun counts letter -> 
                        let n = counts |> Map.tryFind letter |> Option.defaultValue 0
                        counts |> Map.remove letter |> Map.add letter (n + 1)
                        ) Map.empty

let (twos, threes) =
    input
    |> Seq.map countLetters
    |> Seq.fold (fun (twos, threes) count ->
                    let thisTwos =   if count |> Map.exists (fun _ v -> v = 2) then 1 else 0
                    let thisThrees = if count |> Map.exists (fun _ v -> v = 3) then 1 else 0
                    twos + thisTwos, threes + thisThrees
                ) (0,0)

let checksum = twos * threes

printfn "Checksum: %d" checksum