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

    let move (ropePart: RopePart) move =
        let amount = move.Amount
        let xPos = ropePart.xPos
        let yPos = ropePart.yPos

        let newX, newY =
            match move.Direction with
            | Up -> xPos, yPos - amount
            | Down -> xPos, yPos + amount
            | Left -> xPos - amount, yPos
            | Right -> xPos + amount, yPos

        { ropePart with
            xPos = newX
            yPos = newY }
