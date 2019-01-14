open System.Text.RegularExpressions

// let input = [|
//     "position=< 9,  1> velocity=< 0,  2>"
//     "position=< 7,  0> velocity=<-1,  0>"
//     "position=< 3, -2> velocity=<-1,  1>"
//     "position=< 6, 10> velocity=<-2, -1>"
//     "position=< 2, -4> velocity=< 2,  2>"
//     "position=<-6, 10> velocity=< 2, -2>"
//     "position=< 1,  8> velocity=< 1, -1>"
//     "position=< 1,  7> velocity=< 1,  0>"
//     "position=<-3, 11> velocity=< 1, -2>"
//     "position=< 7,  6> velocity=<-1, -1>"
//     "position=<-2,  3> velocity=< 1,  0>"
//     "position=<-4,  3> velocity=< 2,  0>"
//     "position=<10, -3> velocity=<-1,  1>"
//     "position=< 5, 11> velocity=< 1, -2>"
//     "position=< 4,  7> velocity=< 0, -1>"
//     "position=< 8, -2> velocity=< 0,  1>"
//     "position=<15,  0> velocity=<-2,  0>"
//     "position=< 1,  6> velocity=< 1,  0>"
//     "position=< 8,  9> velocity=< 0, -1>"
//     "position=< 3,  3> velocity=<-1,  1>"
//     "position=< 0,  5> velocity=< 0, -1>"
//     "position=<-2,  2> velocity=< 2,  0>"
//     "position=< 5, -2> velocity=< 1,  2>"
//     "position=< 1,  4> velocity=< 2,  1>"
//     "position=<-2,  7> velocity=< 2, -2>"
//     "position=< 3,  6> velocity=<-1, -1>"
//     "position=< 5,  0> velocity=< 1,  0>"
//     "position=<-6,  0> velocity=< 2,  0>"
//     "position=< 5,  9> velocity=< 1, -2>"
//     "position=<14,  7> velocity=<-2,  0>"
//     "position=<-3,  6> velocity=< 2, -1>"
// |]
let input = System.IO.File.ReadAllLines "problem_10.data"

let parseInput lines =
    let reg = Regex(@"^position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>$")
    lines
    |> Array.map (fun line ->
        let px = reg.Replace(line, @"$1") |> System.Int64.Parse
        let py = reg.Replace(line, @"$2") |> System.Int64.Parse
        let vx = reg.Replace(line, @"$3") |> System.Int64.Parse
        let vy = reg.Replace(line, @"$4") |> System.Int64.Parse
        (px,py),(vx,vy)
        )

let printPositions points =
    let positions : (int64*int64)[] = points |> Array.map (fst)
    let minX = positions |> Seq.minBy fst |> fst
    let minY = positions |> Seq.minBy snd |> snd
    let maxX = positions |> Seq.maxBy fst |> fst
    let maxY = positions |> Seq.maxBy snd |> snd
    let xOffset = -minX
    let yOffset = -minY
    let xLength = maxX - minX + 1L
    let yLength = maxY - minY + 1L
    let ps =
        positions
        |> Set.ofArray
        |> Set.map (fun (x,y) -> (x+xOffset) |> int, (y+yOffset) |> int)
    let grid =
        Array2D.init (yLength |> int) (xLength |> int) (fun y x -> Set.contains (x,y) ps)
    for y in 0..((int yLength) - 1) do
        for x in 0..((int xLength) - 1) do
            let c = if grid.[y,x] then '#' else '.'
            printf "%c" c
        printfn ""
    ()

let advance points =
    points |> Array.map (fun ((px,py), (vx,vy)) -> (px+vx,py+vy), (vx,vy))

let getBBLengths points =
    let minX = points |> Seq.map (fun ((x,_),_) -> x) |> Seq.min
    let minY = points |> Seq.map (fun ((_,y),_) -> y) |> Seq.min
    let maxX = points |> Seq.map (fun ((x,_),_) -> x) |> Seq.max
    let maxY = points |> Seq.map (fun ((_,y),_) -> y) |> Seq.max
    let xLength = maxX - minX + 1L
    let yLength = maxY - minY + 1L
    (xLength, yLength)

let getBBArea (xLength,yLength) =
    xLength * yLength

let findMinBB points maxCount =
    let rec inner ps maxCount count (minBBLengths, minBBArea, iterations, minPoints) =
        if count >= maxCount then
            (minBBLengths, minBBArea, iterations, minPoints)
        else
            let newCount = count + 1
            let newPoints = ps |> advance
            let bbLengths = newPoints |> getBBLengths
            let bbArea = bbLengths |> getBBArea
            let newState =
                    if bbArea < minBBArea then
                        (bbLengths, bbArea, newCount, newPoints)
                    else
                        (minBBLengths, minBBArea, iterations, minPoints)
            inner newPoints maxCount newCount newState

    let bbLengths = points |> getBBLengths
    let bbArea = bbLengths |> getBBArea  
    inner points maxCount 0 (bbLengths, bbArea, 0, points)

let points = input |> parseInput

let (xl,yl), minBBArea, iterations, minPoints = findMinBB points 100_000

printfn "Iterations (seconds): %i" iterations
printfn "Min Area: %i" minBBArea
printfn "Lengths: %i, %i" xl yl
printPositions minPoints
printfn "Finished!"
