open System.Diagnostics

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

let buildGrid width height serial =
    Array2D.initBased 1 1 width height <| fun x y -> powerLevel x y serial

let rec powerForSquare xTL yTL size (grid: int[,]) (cache: Map<(int*int*int),int>) =
    if size = 1 then
        (grid.[xTL,yTL], cache)
    else
        match Map.tryFind (xTL,yTL,size) cache with
        | Some powerLevel -> powerLevel, cache
        | None ->
            // top side + left size + sub square one to the south east
            let sides =
                [ for x in xTL..(xTL+size-1) -> (x, yTL) ]
                @
                [ for y in yTL+1..(yTL+size-1) -> (xTL, y) ]
                |> List.map (fun (x,y) -> grid.[x,y])
                |> List.sum
            let subXTL, subYTL, subSize = xTL+1, yTL+1, size-1
            let subPower, subCache = powerForSquare subXTL subYTL subSize grid cache
            let newPower = subPower + sides
            let newCache = subCache |> Map.add (xTL,yTL,size) newPower
            newPower, newCache

let printSquare xTL yTL size (grid: int [,]) =        
    for y in yTL..(yTL+size-1) do
        let values = [ for x in xTL..(yTL+size-1) -> grid.[x,y] ]
        values
        |> List.iter (fun n -> printf " %i" n)
        printfn " | %i" (values |> List.sum)

let width = 300
let height = 300
let serial = 6392

printfn "Building grid..."
let grid = buildGrid width height serial

printfn "Getting top lefts and sizes..."
let topLefts =
    [
        for y in height .. -1 .. 1 do
            for x in width .. -1 .. 1 do
                let maxSize = min (width-x+1) (height-y+1)
                for size in maxSize .. -1 .. 1 do
                    yield (x,y,size)
    ]

printfn "Getting max power level..."
let values, endCache =
    topLefts
    |> List.fold (fun (lst, cache) (x,y,size) ->
        let power, newCache = powerForSquare x y size grid cache
        let value = (x,y,size),power
        value::lst, newCache
        ) ([],Map.empty)

let (mx, my, msize), maxPowerLevel =
    values
    |> Seq.maxBy (snd)    


printfn "Result: x,y,size: %i,%i,%i Total Power: %i" mx my msize maxPowerLevel
