

let powerLevel x y serial =
    let rackId = x + 10
    let value = ((rackId * y) + serial) * rackId
    let digit =
        if value < 100 then
            0
        else
            let digits = value.ToString()
            digits.[digits.Length - 3].ToString() |> System.Int32.Parse
    digit - 5

let powerForSquare xTL yTL serial =
    [        
        for y in yTL..(yTL+2) do
            for x in xTL..(xTL+2) do
                yield (x,y)            
    ]
    |> List.map (fun (x,y) -> powerLevel x y serial)
    |> List.sum

let width = 300
let height = 300
let serial = 6392

let topLefts = [
    for y in 1..(height-3) do
        for x in 1..(width-3) do
            yield (x,y)
]
let maxTL, maxPowerLevel =
    topLefts
    |> List.map (fun (x,y) -> ((x,y), powerForSquare x y serial))
    |> List.maxBy (snd)

printfn "Top Left: %A, Total Power: %i" maxTL maxPowerLevel
