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

let steps : Map<char,(Set<char>*bool)> =    
    let parsed = input |> Seq.map (fun line -> line.[5], line.[36])
    let allSteps =
        parsed
        |> Seq.fold (fun s (a,b) -> s |> Set.add a |> Set.add b) Set.empty
        |> Seq.map (fun step -> step, (Set.empty, false))
        |> Map.ofSeq

    parsed
    |> Seq.fold (fun map (a,b) ->
        let (requirements, _) = map |> Map.find b
        map |> Map.remove b |> Map.add b ((requirements |> Set.add a), false)
        ) allSteps

let rec executeSteps state executed =
    let exSet = executed |> Set.ofList
    let executable =
        state
        |> Map.filter (fun _ (reqs, exed) -> (not exed) && ((reqs - exSet) |> Set.isEmpty))
    if executable |> Map.isEmpty then
        executed |> List.rev
    else
        let nextStep =
            executable
            |> Map.toList
            |> List.map (fun (k,_) -> k)
            |> List.sort
            |> List.head
        let (reqs,_) = executable |> Map.find nextStep
        let newState = state |> Map.remove nextStep |> Map.add nextStep (reqs, true)
        let newExecuted = nextStep::executed
        executeSteps newState newExecuted

let executedSteps = (executeSteps steps []) |> Array.ofList |> System.String

printfn "ExecutedSteps: %s" executedSteps