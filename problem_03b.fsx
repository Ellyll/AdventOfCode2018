open System.Text.RegularExpressions

let input = System.IO.File.ReadAllLines("problem_03.data")

type Claim = { ClaimId: string ; X: int ; Y: int ; Width: int ; Height: int }

let parseClaim line =
    let reg = Regex(@"^#(\S+) @ (\d+),(\d+): (\d+)x(\d+)$")
    if not (reg.IsMatch line) then
        failwithf "Invalid input: %s" line
    {
        ClaimId = reg.Replace(line, "$1")
        X       = reg.Replace(line, "$2") |> int
        Y       = reg.Replace(line, "$3") |> int
        Width   = reg.Replace(line, "$4") |> int
        Height  = reg.Replace(line, "$5") |> int
    }

let drawClaim fabric claim =
    let squares = [
        for y in [ 0..claim.Height-1 ] do
            for x in [ 0..claim.Width-1 ] do
                yield (claim.Y+y, claim.X+x)
    ]
    squares |> List.fold (fun fab sq ->
                            match (fab |> Map.tryFind sq) with
                            | None -> fab |> Map.add sq [ claim.ClaimId ]
                            | Some lst -> fab |> Map.remove sq |> Map.add sq (claim.ClaimId::lst)
                            ) fabric


let claims = input |> Array.map parseClaim
let fabric = claims |> Array.fold (drawClaim) Map.empty

let overlaps = fabric |> Map.filter (fun _ v -> (v |> List.length) >= 2)

let claimsWithOverlaps = overlaps |> Map.fold (fun claimIds _ v ->
                                                v |> List.fold (fun s claimId -> s |> Set.add claimId) claimIds
                                                ) Set.empty

let claimIds = claims |> Array.map (fun claim -> claim.ClaimId) |> Set.ofArray

let result = claimIds - claimsWithOverlaps

printfn "Result: %A" result
