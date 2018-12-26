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

let maxX = (coords |> Array.maxBy (fst) |> fst) + 1
let maxY = (coords |> Array.maxBy (snd) |> snd) + 1
let minX = (coords |> Array.minBy (fst) |> fst) - 1
let minY = (coords |> Array.minBy (snd) |> snd) - 1

let locations =
    [
        for y in minY..maxY do
            for x in minX..maxX -> (x,y)
    ]

let getClosest distances =
    let closestDistance = distances |> Map.toList |> List.minBy (snd) |> snd
    let closest = distances |> Map.filter (fun _ v -> v = closestDistance) |> Map.toList
    match closest with
    | [x] -> Some x
    | _ -> None


let closest =
    locations
    |> List.map (fun p1 ->
            (p1, coords |> Array.map (fun p2 -> (p2, manhattanDistance p1 p2)) |> Map.ofArray |> getClosest)
        )
    |> Map.ofList

let infinates =
    let isEdge (x,y) _ =
        // top, bottom, left, right
        y = minY || y = maxY || x = minX || x = maxX
    let extract (m : Map<int*int, ((int*int)*int) option>) =
        m |> Map.toList |> List.map snd |> List.choose id |> List.map fst |> Set.ofList
    closest |> Map.filter (isEdge) |> extract

let biggestArea =
    closest
    |> Map.toList
    |> List.filter (fun (p1, c) ->
        match c with
        | Some _ -> true
        | None -> false
        )
    |> List.map (fun (p1, d) ->
        match d with
        | Some (p2, _) -> p1, p2
        | _ -> failwith "None where there should be Some"
        )
    |> List.groupBy (fun (p1,p2) -> p2)
    |> List.map (fun (p2, xs) -> (p2, xs |> List.length))
    |> List.filter (fun (p2, _) -> not <| (infinates |> Set.contains p2) )
    |> List.maxBy (snd)

printfn "Biggest area: %A" biggestArea
