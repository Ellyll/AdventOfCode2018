let input = System.IO.File.ReadAllLines "problem_07.data"
// let input = [
//     "Step C must be finished before step A can begin."
//     "Step C must be finished before step F can begin."
//     "Step A must be finished before step B can begin."
//     "Step A must be finished before step D can begin."
//     "Step B must be finished before step E can begin."
//     "Step D must be finished before step E can begin."
//     "Step F must be finished before step E can begin."
// ]
let numberOfWorkers = 5

type StepData = { SecondsRemaining : int ; Worker : int option ; Completed : bool ; Prerequisites : Set<char> }

let getDuration (step: char) : int =
    (int step) - 4 // 'A' is 65 so just need to subtract 4 (A=61, B=62 etc)


let steps : Map<char,StepData> =
    let emptySteps =
        [ 'A'..'Z' ]
        |> List.map (fun step -> step, { SecondsRemaining = getDuration step ; Worker = None ;  Completed = false ; Prerequisites = Set.empty })
        |> Map.ofList

    let parsed = input |> Seq.map (fun line -> line.[5], line.[36])
    parsed
    |> Seq.fold (fun steps (prerequisit, step) ->
            let data = steps |> Map.find step
            steps |> Map.remove step |> Map.add step { data with Prerequisites = (data.Prerequisites |> Set.add prerequisit) }
        ) emptySteps

let rec executeSteps state timeTaken workersAvailable =
    let completed, notCompleted = state |> Map.partition (fun _ v -> v.Completed)
    if (notCompleted |> Map.count) = 0 then // all done
        timeTaken
    else
        let stepsCompleted = completed |> Map.toList |> List.map (fst) |> Set.ofList

        // Allocate any free workers
        let stateAfterAllocation, workersAvailableAfterAllocation =
            if Set.isEmpty workersAvailable then
                state, workersAvailable
            else
                let availableSteps =
                    notCompleted
                    |> Map.filter (fun _ v -> v.Worker = None && ((v.Prerequisites - stepsCompleted) |> Set.count) = 0)
                    |> Map.toList
                    |> List.map (fst)
                    |> List.sort
                let numberToAllocate = min (List.length availableSteps) (Set.count workersAvailable)
                
                availableSteps
                |> List.take numberToAllocate
                |> List.fold (fun (st, wa) step ->
                        let worker = wa |> Set.minElement
                        let data = st |> Map.find step
                        st |> Map.remove step |> Map.add step { data with Worker = Some worker },
                        wa |> Set.remove worker
                    ) (state, workersAvailable)

        // Execute
        let executable =
            stateAfterAllocation
            |> Map.filter (fun _ data -> data.Worker <> None)
        let stateAfterExecution, workersAvailableAfterExecution =
            executable
            |> Map.fold (fun (st, wa) step data ->
                    let remaining = data.SecondsRemaining - 1
                    if remaining = 0 then // finished the step
                        st |> Map.remove step |> Map.add step { data with SecondsRemaining = 0 ; Completed = true ; Worker = None },
                        match data.Worker with
                        | Some worker -> wa |> Set.add worker // add worker back to the pool
                        | None -> failwithf "Step %c executed without worker!" step
                    else
                        st |> Map.remove step |> Map.add step { data with SecondsRemaining = remaining },
                        wa
                ) (stateAfterAllocation, workersAvailableAfterAllocation)

        executeSteps stateAfterExecution (timeTaken + 1) workersAvailableAfterExecution



let workers = [ 1..numberOfWorkers ] |> Set.ofList
let result = executeSteps steps 0 workers
printfn "Result: %i" result
