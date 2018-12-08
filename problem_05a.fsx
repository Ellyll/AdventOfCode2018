let input = (System.IO.File.ReadAllText "problem_05.data").Trim()

let rec react str idx =
    let maxIdx = (str |> String.length)-1
    if idx = maxIdx then
        str
    else
        let a = str.[idx]
        let b = str.[idx+1]
        if System.Char.ToLower(a) = System.Char.ToLower(b) && a <> b then
            let newStr = str.[0..idx-1] + if idx+2 <= maxIdx then str.[idx+2..] else ""
            react newStr (max 0 (idx-1))
        else
            react str (idx+1)

let result = (react input 0) |> String.length
printfn "Result: %d" result
