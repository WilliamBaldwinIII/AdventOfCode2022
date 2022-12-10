#load "FileHelpers.fs"
#load "Helpers.fs"

open Helpers


let fileName = $"9-ex"

let fileLines = FileHelpers.readNumFile fileName


let mainGrid: bool [,] = Array2D.init 100 100 (fun x y -> false)

type RopePart = { xPos: int; yPos: int }


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

    let moveDiagonal (head: RopePart) (tail: RopePart) =
        let headAdjacent = getDirectlyAdjacent head |> set
        let tailAdjacent = getAllAdjacent tail |> set

        let newX, newY =
            Set.intersect headAdjacent tailAdjacent
            |> Seq.exactlyOne

        newX, newY


    let moveTail (head: RopePart) (tail: RopePart) =
        if areAdjacent head tail then
            tail
        else
            let newX, newY =
                if head.xPos = tail.xPos then
                    if head.yPos > tail.yPos then
                        tail.xPos, tail.yPos - 1
                    else
                        tail.xPos, tail.yPos - 1
                elif head.yPos = tail.yPos then
                    if head.xPos > tail.xPos then
                        tail.xPos + 1, tail.yPos
                    else
                        tail.xPos - 1, tail.yPos
                else // Diagonal
                    moveDiagonal head tail

            mainGrid[newX, newY] <- true

            { tail with xPos = newX; yPos = newY }


    let move (head: RopePart, tail: RopePart) move =
        let amount = move.Amount
        let xPos = head.xPos
        let yPos = head.yPos

        let newX, newY =
            match move.Direction with
            | Up -> xPos, yPos - amount
            | Down -> xPos, yPos + amount
            | Left -> xPos - amount, yPos
            | Right -> xPos + amount, yPos

        let positionList =
            [ for x in xPos..newX do
                  for y in yPos..newY do
                      x, y ]

        let foldFn (head, tail) (x, y) =

            let newHead = { head with xPos = x; yPos = y }

            let newTail = moveTail newHead tail

            newHead, newTail


        let newHead, newTail = positionList |> List.fold foldFn (head, tail)

        newHead, newTail



let initialX = 50
let initialY = 50

let head = { xPos = initialX; yPos = initialY }
let tail = { xPos = initialX; yPos = initialY }

let moves = fileLines |> List.map Move.parseLine

moves |> List.fold Move.move (head, tail)

let numPositionsTailTouched =
    mainGrid
    |> Array2D.flatten
    |> Seq.filter id
    |> Seq.length
