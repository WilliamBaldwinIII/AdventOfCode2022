#load "FileHelpers.fs"
#load "Helpers.fs"

open Helpers


let fileName = $"9-ex"

let fileLines = FileHelpers.readNumFile fileName

let mainGrid: bool [,] = Array2D.init 100 100 (fun x y -> false)

let initialX = 50
let initialY = 50

type RopePart = { xPos: int; yPos: int }

let head = { xPos = initialX; yPos = initialY }
let tail = { xPos = initialX; yPos = initialY }

type Direction =
    | Up
    | Down
    | Left
    | Right

module Direction =
    let create =
        String.trim
        >> function
            | "u" -> Up
            | "d" -> Down
            | "l" -> Left
            | "r" -> Right
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

        xDiff <= 1 && yDiff <= 1

    let moveTail (head: RopePart) (tail: RopePart) =
        if areAdjacent head tail then
            tail
        else
            // TODO
            tail

    let move (head: RopePart) (tail: RopePart) move =
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


let moves = fileLines |> List.map Move.parseLine
raise
