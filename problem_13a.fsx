type Facing =
    | Up
    | Down
    | Left
    | Right

type ClockDirection =
    | Clockwise
    | AntiClockwise

type Cart = { X: int ; Y: int ; Facing: Facing ; Intersections: int }

type CartMoveResult =
    | Success of Cart
    | CrashedIntoOtherCart of Cart

type CartsMoveResult =
    | Success of Cart list
    | Crash of (Cart * Cart)

let linesToGrid (lines: string[]) =
    let width = String.length lines.[0]
    let height = Array.length lines
    let data =
        seq {
            for y in 0..(height-1) do
                for x in 0..(width-1) ->
                    (x,y,lines.[y].[x])
        }
    let grid = Array2D.create width height ' '
    // Get cards and populate grid
    let cartSymbols = Set.ofSeq "<^>v"
    let carts =
        data |>
        Seq.fold (fun cs (x,y,symbol) ->
            let newCs, newSymbol =                    
                if Set.contains symbol cartSymbols then
                    // It's a cart, so add to list and replace with appropriate track
                    let facing =
                        match symbol with
                        | '<' -> Left
                        | '>' -> Right
                        | '^' -> Up
                        | 'v' -> Down
                        | c -> failwithf "Invalid symbol: %c" c
                    ({ X = x ; Y = y ; Facing = facing ; Intersections = 0 }::cs,
                        match facing with
                        | Left
                        | Right -> '-'
                        | Up
                        | Down -> '|')
                else
                    (cs, symbol)
            grid.[x,y] <- newSymbol
            newCs
            ) []
    (grid, carts)

let printGrid (grid: char[,]) (carts: seq<Cart>) =
    let gridWithCarts = Array2D.copy grid
    carts
    |> Seq.iter (fun cart ->
            let symbol =
                match cart.Facing with
                | Up -> '^'
                | Down -> 'v'
                | Left -> '<'
                | Right -> '>'
            gridWithCarts.[cart.X,cart.Y] <- symbol
        )
    for y in 0..(Array2D.length2 grid)-1 do
        for x in 0..(Array2D.length1 grid)-1 do
            printf "%c" gridWithCarts.[x,y]
        printfn ""

let moveCartForwards cart =
    let newX, newY =
        match cart.Facing with
        | Up -> cart.X, cart.Y-1
        | Down -> cart.X, cart.Y+1
        | Left -> cart.X-1, cart.Y
        | Right -> cart.X+1, cart.Y
    { cart with X = newX ; Y = newY }

let turnCart clockDirection cart =
    let newFacing =
        match clockDirection with
        | Clockwise ->
            match cart.Facing with
            | Up -> Right
            | Right -> Down
            | Down -> Left
            | Left -> Up
        | AntiClockwise ->
            match cart.Facing with
            | Up -> Left
            | Left -> Down
            | Down -> Right
            | Right -> Up
    { cart with Facing = newFacing }

let moveCart (cart: Cart) (carts: Cart list) (grid: char[,]): CartMoveResult =
    let symbol = grid.[cart.X, cart.Y]
    let turnedCart =
        match symbol with
        | '/' when cart.Facing = Up    -> cart |> turnCart Clockwise
        | '/' when cart.Facing = Down  -> cart |> turnCart Clockwise
        | '/' when cart.Facing = Left  -> cart |> turnCart AntiClockwise
        | '/' when cart.Facing = Right -> cart |> turnCart AntiClockwise
        | '\\' when cart.Facing = Up    -> cart |> turnCart AntiClockwise
        | '\\' when cart.Facing = Down  -> cart |> turnCart AntiClockwise
        | '\\' when cart.Facing = Left  -> cart |> turnCart Clockwise
        | '\\' when cart.Facing = Right -> cart |> turnCart Clockwise
        | '+' when cart.Intersections = 0 ->  { (cart |> turnCart AntiClockwise) with Intersections = 1 } // turn left
        | '+' when cart.Intersections = 1 ->  { cart with Intersections = 2 } // straight ahead
        | '+' when cart.Intersections = 2 ->  { (cart |> turnCart Clockwise) with Intersections = 0 } // turn right
        | _ -> cart
    let movedCart = turnedCart |> moveCartForwards
    let cartAtSameLocation =
        carts
        |> List.tryFind (fun c -> c <> cart && c.X = movedCart.X && c.Y = movedCart.Y)
    match cartAtSameLocation with
    | Some c -> CrashedIntoOtherCart c
    | None -> CartMoveResult.Success movedCart

let moveCarts carts grid =
    let sortedCarts =
        carts
        |> List.sortBy (fun c -> (c.Y,c.X))
    let rec loop remaining currentCarts =
        match remaining with
        | [] -> Success currentCarts
        | cart::rest ->
            match moveCart cart currentCarts grid with
            | CrashedIntoOtherCart target -> Crash (cart,target)
            | CartMoveResult.Success movedCart ->
                let newCarts =
                    movedCart::(currentCarts |> List.filter (fun c -> c <> cart))
                loop rest newCarts
    loop sortedCarts sortedCarts

let moveUntilCrashed grid carts =
    let rec loop currentCarts ticks =
        match moveCarts currentCarts grid with
        | Crash (source, target) -> (target.X, target.Y)
        | Success newCarts -> loop newCarts (ticks+1)
    loop carts 0


let input = System.IO.File.ReadAllLines "problem_13.data"
let grid, carts = input |> linesToGrid

let result = moveUntilCrashed grid carts
printfn "Result: %A" result
