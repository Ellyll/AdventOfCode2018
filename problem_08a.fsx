open System

//let input = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
let input = (System.IO.File.ReadAllText "problem_08.data").Trim()

type Node = { Children: Node list ; Metadata: int list }


let values =
    input.Split [| ' ' |]
    |> Array.map (Int32.Parse)
    |> Array.toList

let rec readNode (data: int list) : Node*(int list) =
    match data with
    | nChild::nMeta::rest ->
        let children, remaining =
            if nChild = 0 then
                [], rest
            else
                [ 1..nChild ]
                |> List.fold (fun (cs,rem) _ ->
                    let child, r = readNode rem
                    child::cs,r) ([],rest)
        let metadata, remaining2 =
            remaining |> List.splitAt nMeta
        ({ Children = children ; Metadata = metadata }, remaining2)
    | _ -> failwith "Unexpected data reading node"

let sumMetadata node =
    let rec inner nd total =
        let newTotal = total + (nd.Metadata |> List.sum)
        match nd.Children with
        | [] -> newTotal
        | xs -> xs |> List.fold (fun tot x -> tot + (inner x 0)) newTotal

    inner node 0


let node, remaining = readNode values
let result = sumMetadata node

printfn "Result: %i" result