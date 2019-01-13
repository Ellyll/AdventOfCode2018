// Mutable cyclic linked list - I didn't know how to make an immutable one, but it probable wouldnt work performance-wise anyway
module Cyclic =
    type Node<'T> = { mutable Value: 'T ; mutable Previous: Node<'T> ; mutable Next: Node<'T> }

    let insertAfter (value: 'T) (currentNode: Node<'T>) : Node<'T> =
        let newNode = { Value = value ; Previous = currentNode ; Next = currentNode.Next }
        currentNode.Next <- newNode
        newNode.Next.Previous <- newNode
        newNode

    let fold (folder: 'State->Node<'T>->'State) (state: 'State) (node: Node<'T>) : 'State =
        let rec inner start current currentState =
            if System.Object.ReferenceEquals(current, start) then
                currentState
            else
                let newState = folder currentState current
                inner start current.Next newState
        let startingState = folder state node
        inner node node.Next startingState

    let rec move (steps: int) (node: Node<'T>) : Node<'T> =
        if steps = 0 then
            node
        elif steps > 0 then
            move (steps - 1) node.Next
        else
            move (steps + 1) node.Previous

    let ofList (list: 'T list) : Node<'T> =
        match list with
        | [] -> failwithf "list cannot be empty"
        | _ ->
            let rec dummy = { Value = List.head list ; Previous = dummy ; Next = dummy }
            let nodes = list |> List.map (fun x -> { Value = x ; Previous = dummy ; Next = dummy })
            let maxI = (nodes |> List.length) - 1
            nodes
            |> List.iteri (fun i n ->
                let nextIndex = if i = maxI then 0 else i + 1
                let prevIndex = if i = 0 then maxI else i - 1
                let nextNode = nodes |> List.item nextIndex
                let prevNode = nodes |> List.item prevIndex
                n.Next <- nextNode
                n.Previous <- prevNode
                )
            nodes |> List.head

        
    let remove (currentNode: Node<'T>) : 'T*Node<'T> =
        if System.Object.ReferenceEquals(currentNode.Next, currentNode) then
            failwith "Cannot delete last remaining node"
        let value = currentNode.Value
        let previous = currentNode.Previous
        previous.Next <- currentNode.Next
        currentNode.Next.Previous <- previous        
        (value, previous)

    let singleton (value: 'T) : Node<'T> =
        let rec node = { Value = value ; Previous = node ; Next = node }
        node

    let toList (node: Node<'T>) : 'T list =
        node |> fold (fun lst node -> node.Value::lst) [] |> List.rev
