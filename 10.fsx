#load "FileHelpers.fs"
#load "Helpers.fs"

open Helpers
open System

//fsi.ShowDeclarationValues <- false

let fileName = $"10-ex"

let fileLines = FileHelpers.readNumFile fileName

let maxCycle = 220

let isCountCycle cycle = (cycle - 20) % 40 = 0

type Move =
    | Noop
    | AddX of int

let moveCycleCount =
    function
    | Noop -> 1
    | AddX _ -> 2

let parseMove (line: string) =
    let lineArr = line |> String.split " "

    match lineArr with
    | [| "addx"; num |] -> num |> int |> AddX
    | [| "noop" |] -> Noop
    | other -> failwith $"Invalid move! {other}"

let moves = fileLines |> List.map parseMove

type Cycle =
    { CurrentCycle: int
      CurrentSignalStrengthSum: int
      SignalStrenths: int list }

let processMove (cycle: Cycle) (move: Move) = 
        
    let rec runCycle (numCyclesRemainingForMove: int) = 
        let signalStrength = 
            if isCountCycle cycle.CurrentCycle then cycle.CurrentSignalStrengthSum

    cycle

let startCycle =
    { CurrentCycle = 0
      CurrentSignalStrengthSum = 0
      SignalStrenths = [] }

let endCycle = moves |> List.fold processMove startCycle

let signalStrengthSum = endCycle.SignalStrenths |> List.sum

printfn $"Part 1: {signalStrengthSum}"
