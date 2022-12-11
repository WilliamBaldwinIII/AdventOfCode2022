#load "FileHelpers.fs"
#load "Helpers.fs"

open Helpers
open System

//fsi.ShowDeclarationValues <- false

let fileName = $"9"

let fileLines = FileHelpers.readNumFile fileName

let initialX = 250
let initialY = 250


let mainGrid: bool [,] = Array2D.init 500 500 (fun x y -> false)

let resetGrid () =
    mainGrid
    |> Array2D.iteri (fun x y _ -> mainGrid[x, y] <- false)


type RopePart = { xPos: int; yPos: int }

let printGridSection (head: RopePart) (tail: RopePart) =
    for y in initialY - 10 .. initialY + 10 do
        for x in initialX - 10 .. initialX + 10 do
            if x = head.xPos && y = head.yPos then
                Console.Write "H"
            elif x = tail.xPos && y = tail.yPos then
                Console.Write "T"
            elif mainGrid[x, y] then
                Console.Write "#"

            else
                Console.Write "."

        Console.WriteLine ""

    Console.WriteLine ""

let printGridSection' (head: RopePart) (tail: RopePart list) =
    let rec getTailnum (xPos, yPos) curNum tail' =
        match tail' with
        | [] -> None
        | x :: xs ->
            if x.xPos = xPos && x.yPos = yPos then
                Some curNum
            else
                getTailnum (xPos, yPos) (curNum + 1) xs

    for y in initialY - 10 .. initialY + 10 do
        for x in initialX - 10 .. initialX + 10 do
            let tailNum = getTailnum (x, y) 1 tail

            match tailNum with
            | Some t -> Console.Write($"{t}")

            | None ->
                if x = head.xPos && y = head.yPos then
                    Console.Write "H"
                //elif x = tail.xPos && y = tail.yPos then
                //    Console.Write "T"
                elif mainGrid[x, y] then
                    Console.Write "#"
                else
                    Console.Write "."

        Console.WriteLine ""

    Console.WriteLine ""

type Direction =
    | Up
    | Down
    | Left
    | Right

module Direction =
    let create =
        String.trim
        >> function
            | "U" -> Up
            | "D" -> Down
            | "L" -> Left
            | "R" -> Right
            | other -> failwith $"Invalid Direction: {other}"

type Move = { Direction: Direction; Amount: int }

module Move =
    let toString (move: Move) =
        $"{move.Direction.ToString()} {move.Amount}"

    let parseLine (line: string) =
        match String.split " " line with
        | [| direction; amount |] ->
            let direction' = Direction.create direction
            let amount' = int amount

            { Direction = direction'
              Amount = amount' }

        | _ -> failwith $"Can't parse line: {line}"

    let areAdjacent (head: RopePart) (tail: RopePart) =
        let xDiff = abs (head.xPos - tail.xPos)
        let yDiff = abs (head.yPos - tail.yPos)

        if xDiff > 2 || yDiff > 2 then
            failwith $"Head and tail are too far apart! Head: %A{head}, Tail: %A{tail}"

        xDiff <= 1 && yDiff <= 1

    let getAllAdjacent (ropePart: RopePart) =
        [ for x in ropePart.xPos - 1 .. ropePart.xPos + 1 do
              for y in ropePart.yPos - 1 .. ropePart.yPos + 1 do
                  (x, y) ]

    let getDirectlyAdjacent (ropePart: RopePart) =
        [ for x in ropePart.xPos - 1 .. ropePart.xPos + 1 do
              for y in ropePart.yPos - 1 .. ropePart.yPos + 1 do
                  if x = ropePart.xPos || y = ropePart.yPos then
                      (x, y) ]


    let moveTail setVisited (head: RopePart) (tail: RopePart) =
        if areAdjacent head tail then
            if setVisited then
                mainGrid[tail.xPos, tail.yPos] <- true

            tail
        else
            let newX, newY =
                let headAdjacent = getDirectlyAdjacent head |> set
                let tailAdjacent = getAllAdjacent tail |> set

                let intersection =
                    Set.intersect headAdjacent tailAdjacent
                    |> Seq.tryExactlyOne

                let newX, newY =
                    match intersection with
                    | None ->
                        // If we didn't find any directly adjacent, expand to diagonals
                        let headAdjacent = getAllAdjacent head |> set

                        let x, y =
                            Set.intersect headAdjacent tailAdjacent
                            |> Seq.exactlyOne

                        x, y
                    | Some (x, y) -> x, y

                newX, newY

            if setVisited then
                mainGrid[newX, newY] <- true

            { tail with xPos = newX; yPos = newY }


    let move (head: RopePart, tail: RopePart list) move =
        let amount = move.Amount
        let xPos = head.xPos
        let yPos = head.yPos

        let newX, newY =
            match move.Direction with
            | Up -> xPos, yPos - amount
            | Down -> xPos, yPos + amount
            | Left -> xPos - amount, yPos
            | Right -> xPos + amount, yPos

        let xPositions =
            if xPos < newX then
                [ xPos..newX ]
            else
                List.rev [ newX..xPos ]

        let yPositions =
            if yPos < newY then
                [ yPos..newY ]
            else
                List.rev [ newY..yPos ]

        let positionList =
            [ for x in xPositions do
                  for y in yPositions do
                      x, y ]

        let foldFn (head, tail) (x, y) =

            let newHead = { head with xPos = x; yPos = y }

            let newTail = moveTail true newHead tail

            //printGridSection newHead newTail
            newHead, newTail

        let foldManyFn (head, tail) (x, y) =

            let newHead = { head with xPos = x; yPos = y }

            let rec moveTail' newTail currentHead currentTail =
                match currentTail with
                | [] -> newTail
                | currentRopePart :: xs ->
                    let setVisited = xs |> List.isEmpty // Only set visited when the last rope part touches

                    let newRopePart = moveTail setVisited currentHead currentRopePart
                    moveTail' (newTail @ [ newRopePart ]) newRopePart xs

            let newTail = moveTail' [] newHead tail

            //printGridSection' newHead newTail
            newHead, newTail


        //Console.WriteLine $"{move |> toString}"
        let newHead, newTail = positionList |> List.fold foldManyFn (head, tail)

        //Console.WriteLine "Done with turn!"

        //printGridSection' newHead newTail
        //Console.WriteLine "----------------"


        newHead, newTail




let head = { xPos = initialX; yPos = initialY }
let tail = { xPos = initialX; yPos = initialY }

let moves = fileLines |> List.map Move.parseLine

moves |> List.fold Move.move (head, [ tail ])

let numPositionsTailTouched =
    mainGrid
    |> Array2D.flatten
    |> Seq.filter id
    |> Seq.length


Console.WriteLine $"Part 1 -- Num positions: {numPositionsTailTouched}"

// Part 2
let multiTail =
    [ for _ in 1..9 do
          { xPos = initialX; yPos = initialY } ]

resetGrid ()
moves |> List.fold Move.move (head, multiTail)

let numPositionsTailTouchedPt2 =
    mainGrid
    |> Array2D.flatten
    |> Seq.filter id
    |> Seq.length

Console.WriteLine $"Part 2 -- Num positions: {numPositionsTailTouchedPt2}"
