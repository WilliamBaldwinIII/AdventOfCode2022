#load "FileHelpers.fs"
#load "Helpers.fs"

open Helpers
open System

fsi.ShowDeclarationValues <- false
fsi.ShowProperties <- false

let fileName = $"10"

let fileLines = FileHelpers.readNumFile fileName

let getCyclePosition cycle = cycle % 40
let isNewLine cycle = (getCyclePosition (cycle)) = 0
let isCountCycle cycle = (getCyclePosition (cycle - 20)) = 0


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

let processMove (cycle: Cycle) (move: Move) =
    let isSpriteInCurrentPosition currentCycle currentSignalStrengthSum =
        [ currentSignalStrengthSum .. currentSignalStrengthSum + 2 ]
        |> List.contains (getCyclePosition currentCycle)


    let rec runCycle currentCycle (numCyclesRemainingForMove: int) signalStrengths =
        if isSpriteInCurrentPosition currentCycle cycle.CurrentSignalStrengthSum then
            Console.Write "#"
        else
            Console.Write "."

        if isNewLine currentCycle then
            Console.WriteLine()

        let signalStrength =
            if isCountCycle currentCycle then
                (cycle.CurrentSignalStrengthSum * currentCycle)
                |> Some
            else
                None

        let signalStrengths' =
            signalStrength
            |> Option.map (fun s -> s :: signalStrengths)
            |> Option.defaultValue signalStrengths


        if numCyclesRemainingForMove <= 1 then
            currentCycle, signalStrengths'
        else

            runCycle (currentCycle + 1) (numCyclesRemainingForMove - 1) signalStrengths'

    let moveCycleCount = moveCycleCount move

    let newCycle, signalStrengths =
        runCycle cycle.CurrentCycle moveCycleCount cycle.SignalStrengths

    let newTotal =
        match move with
        | Noop -> cycle.CurrentSignalStrengthSum
        | AddX num -> cycle.CurrentSignalStrengthSum + num


    { CurrentCycle = newCycle + 1
      CurrentSignalStrengthSum = newTotal
      SignalStrengths = signalStrengths }

let startCycle =
    { CurrentCycle = 1
      CurrentSignalStrengthSum = 1
      SignalStrengths = [] }

let endCycle = moves |> List.fold processMove startCycle

let signalStrengthSum = endCycle.SignalStrengths |> List.sum

printfn ""
printfn $"Part 1: {signalStrengthSum}"
