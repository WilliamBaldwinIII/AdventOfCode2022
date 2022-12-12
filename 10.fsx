﻿#load "FileHelpers.fs"
#load "Helpers.fs"

open Helpers
open System

//fsi.ShowDeclarationValues <- false

let fileName = $"10-ex"

let fileLines = FileHelpers.readNumFile fileName

let maxCycle = 220

let isCountCycle cycle =
    cycle <= maxCycle && (cycle - 20) % 40 = 0

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
      SignalStrengths: int list }

type PendingMove =
    { Move: Move
      NumCyclesRemainingForMove: int }


let processMove (cycle: Cycle) (move: Move) =

    Console.WriteLine(
        $"processMove: BEFORE currentCycle {cycle.CurrentCycle}, CurrentSignalStrengthSum {cycle.CurrentSignalStrengthSum}, signalStrengths = {cycle.SignalStrengths} "
    )

    Console.WriteLine($"processMove: {move}")

    let rec runCycle currentCycle (numCyclesRemainingForMove: int) signalStrengths =
        if numCyclesRemainingForMove <= 0 then
            Console.WriteLine($"runCycle: returning")

            Console.WriteLine(
                $"runCycle: currentCycle {currentCycle}, numCyclesRemainingForMove {numCyclesRemainingForMove}, signalStrengths = {signalStrengths} "
            )

            currentCycle, signalStrengths
        else
            let signalStrength =
                if isCountCycle currentCycle then
                    (cycle.CurrentSignalStrengthSum * currentCycle)
                    |> Some
                else
                    None

            let signalStrengths =
                signalStrength
                |> Option.map (fun s -> s :: cycle.SignalStrengths)
                |> Option.defaultValue cycle.SignalStrengths

            Console.WriteLine(
                $"runCycle: currentCycle {currentCycle}, numCyclesRemainingForMove {numCyclesRemainingForMove}, signalStrengths = {signalStrengths} "
            )

            runCycle (currentCycle + 1) (numCyclesRemainingForMove - 1) signalStrengths

    let moveCycleCount = moveCycleCount move

    let newCycle, signalStrengths =
        runCycle cycle.CurrentCycle moveCycleCount cycle.SignalStrengths

    let newTotal =
        match move with
        | Noop -> cycle.CurrentSignalStrengthSum
        | AddX num -> cycle.CurrentSignalStrengthSum + num

    Console.WriteLine(
        $"processMove: AFTER currentCycle {newCycle}, CurrentSignalStrengthSum {newTotal}, signalStrengths = {signalStrengths} "
    )

    let pendingMoves: PendingMove list = []

    let newTotal =
        pendingMoves
        |> List.filter (fun p -> p.NumCyclesRemainingForMove <= 0)
        |> List.sumBy (fun p ->
            match p.Move with
            | AddX num -> num
            | Noop -> 0)

    let newPendingMove =
        { Move = move
          NumCyclesRemainingForMove = moveCycleCount }


    let newPendingMoves =
        newPendingMove
        :: (pendingMoves
            |> List.filter (fun p -> p.NumCyclesRemainingForMove > 0))

    { CurrentCycle = newCycle + 1
      CurrentSignalStrengthSum = newTotal
      SignalStrengths = signalStrengths }

let startCycle =
    { CurrentCycle = 1
      CurrentSignalStrengthSum = 1
      SignalStrengths = [] }

let endCycle = moves |> List.fold processMove startCycle

let signalStrengthSum = endCycle.SignalStrengths |> List.sum

printfn $"Part 1: {signalStrengthSum}"
