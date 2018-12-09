let input = (System.IO.File.ReadAllText "problem_05.data").Trim()

let react str =
    let rec inner str idx =
        let maxIdx = (str |> String.length)-1
        if idx = maxIdx then
            str
        else
            let a = str.[idx]
            let b = str.[idx+1]
            if System.Char.ToLower(a) = System.Char.ToLower(b) && a <> b then
                let newStr = str.[0..idx-1] + if idx+2 <= maxIdx then str.[idx+2..] else ""
                inner newStr (max 0 (idx-1))
            else
                inner str (idx+1)
    inner str 0            

let remove c str =
    let lowerC = System.Char.ToLower c
    str |> String.filter (fun x -> System.Char.ToLower x <> lowerC)

let _, result =
    [ 'a'..'z' ]
    |> List.map (fun c -> c, input |> remove c |> react |> String.length)
    |> List.minBy (snd)

printfn "Result: %d" result
