open System.Text.RegularExpressions
open System
open System.Globalization

let input = System.IO.File.ReadAllLines "problem_04.data"

type LogActivity =
    | BeginShift
    | FallsAsleep
    | WakesUp

type LogEntry = { Timestamp: DateTime ; GuardId: int ; Activity: LogActivity ; }

let logEntries, _ : (LogEntry list * int option) =
    input
    |> Array.sort
    |> Array.fold (fun (entries,prevGuardId) line ->
        let timestamp =  DateTime.ParseExact(line.[1..16], "yyyy-MM-dd HH:mm", System.Globalization.CultureInfo.CurrentCulture, DateTimeStyles.None)
        let reg = Regex @"^Guard #(\d+) begins shift$"
        let activityStr = line.[19..]
        let activity =
            if activityStr = "falls asleep" then FallsAsleep
            elif activityStr = "wakes up" then WakesUp
            else                    
                if not <| reg.IsMatch activityStr then
                    failwithf "Unrecognised activity: %s" activityStr
                BeginShift
        let guardId =
            match activity with
            | BeginShift -> int <| reg.Replace(activityStr, "$1")
            | FallsAsleep
            | WakesUp ->
                match prevGuardId with
                | None -> failwith "No previous guardId"
                | Some gId -> gId
        
        ({ Timestamp = timestamp ; GuardId = guardId ; Activity = activity }::entries, Some guardId)
        ) ([], None)

let sleeps =
    logEntries
    |> List.sortBy (fun logEntry -> logEntry.Timestamp)
    |> List.fold (fun (slps, prev) entry ->
            match entry.Activity with
                | BeginShift ->
                    match prev with
                    | None -> (slps, Some (entry.GuardId, None))
                    | Some (_, None) -> (slps, Some (entry.GuardId, None))
                    | Some (guardId, Some timestamp)
                        -> ((guardId, timestamp, entry.Timestamp.AddMinutes(-1.0))::slps), Some (entry.GuardId, None)
                | FallsAsleep ->
                    slps, Some(entry.GuardId, Some entry.Timestamp)
                | WakesUp ->
                    match prev with
                    | Some (guardId, Some timestamp)
                        -> ((guardId, timestamp, entry.Timestamp.AddMinutes(-1.0))::slps), Some(entry.GuardId, None)
                    | _ -> failwithf "Invalid entry: %A" entry
            ) ([], None)
        |> fst
        |> List.map (fun (guardId, startTime, endTime) -> guardId, startTime, endTime, (endTime - startTime).Minutes+1)
        |> List.sortBy (fun (guardId,startTime,_,_) -> (guardId,startTime))

let minute, guardId, _ =
    sleeps
    |> List.collect (fun (guardId,startTime, _, duration) ->
        [ for m in 0..(duration-1) -> startTime.AddMinutes(float m).Minute, guardId ]
        )
    |> List.groupBy (fun x -> x)
    |> List.map (fun ((minute, guardId), xs) -> minute, guardId, (xs |> List.length))
    |> List.maxBy (fun (_, _, count) -> count)

let result = guardId * minute

printfn "GuardID: %d" guardId
printfn "Minute: %d" minute
printfn "Result: %d" result
