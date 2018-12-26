open System

// let input = [|
//     "1, 1"
//     "1, 6"
//     "8, 3"
//     "3, 4"
//     "5, 5"
//     "8, 9"
// |]
let input = System.IO.File.ReadAllLines "problem_06.data"

let manhattanDistance (x1,y1) (x2,y2) =
    abs (x1-x2) + abs (y1-y2)

let area (x1,y1) (x2,y2) =
    (abs (x2-x1)) * (abs (y2-y1))

let parseCoord (str: string) =
    let values = str.Split [| ',' |] |> Array.map (fun s -> (s.Trim()) |> int)
    (values.[0], values.[1])

let coords =
    input
    |> Array.map (parseCoord)
    |> Set.ofArray

let maxX = (coords |> Seq.maxBy (fst) |> fst) + 1
let maxY = (coords |> Seq.maxBy (snd) |> snd) + 1
let minX = (coords |> Seq.minBy (fst) |> fst) - 1
let minY = (coords |> Seq.minBy (snd) |> snd) - 1

let locations =
    [
        for y in minY..maxY do
            for x in minX..maxX -> (x,y)
    ]
    |> Set.ofList

let locationsWithinDistance =
    locations
    |> Set.map (fun p -> p, (coords |> Seq.sumBy (manhattanDistance p)))
    |> Set.filter (fun (_,distance) -> distance < 10000)

let result = locationsWithinDistance |> Set.count

printfn "Result: %i" result
