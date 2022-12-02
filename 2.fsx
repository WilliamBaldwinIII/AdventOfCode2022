#load "Helpers.fs"

open System

let fileName = $"2-ex"

let lines = Helpers.readNumFile fileName

type Shape =
    | Rock
    | Paper
    | Scissors

module Shape =
    let create =
        function
        | "A"
        | "X" -> Rock
        | "B"
        | "Y" -> Paper
        | "C"
        | "Z" -> Scissors
        | invalid -> failwith $"Invalid shape input: {invalid}"

    let points =
        function
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3

type Round =
    | Win
    | Loss
    | Draw

module Round =
    let points =
        function
        | Win -> 6
        | Draw -> 3
        | Loss -> 0

    let create =
        function
        | "X" -> Loss
        | "Y" -> Draw
        | "Z" -> Win
        | invalid -> failwith $"Invalid Round input: {invalid}"

    let result mine theirs =
        match mine, theirs with
        | Rock, Paper -> Loss
        | Rock, Scissors -> Win
        | Rock, Rock -> Draw
        | Paper, Paper -> Draw
        | Paper, Scissors -> Loss
        | Paper, Rock -> Win
        | Scissors, Paper -> Win
        | Scissors, Scissors -> Draw
        | Scissors, Rock -> Loss

    let resultShape theirs desiredResult =
        match theirs, desiredResult with
        | Rock, Loss -> Scissors
        | Rock, Win -> Paper
        | Rock, Draw -> Rock
        | Paper, Draw -> Paper
        | Paper, Loss -> Rock
        | Paper, Win -> Scissors
        | Scissors, Win -> Rock
        | Scissors, Draw -> Scissors
        | Scissors, Loss -> Paper

let roundPointTotal mine theirs =
    let result = Round.result mine theirs
    Round.points result + Shape.points mine

let getLineContents (line: string) =
    let splitLine = line.Split(' ', StringSplitOptions.RemoveEmptyEntries)
    string splitLine[0], string splitLine[1]

let parseLine (line: string) =
    let theirs, mine = getLineContents line
    let theirs = theirs |> Shape.create
    let mine = mine |> Shape.create

    theirs, mine

let runRound (line: string) =
    let line = line.Trim()
    let theirs, mine = parseLine line

    (mine, theirs) ||> roundPointTotal


let allRounds = lines |> List.map runRound

let roundSum = allRounds |> List.sum


printfn "Total round sum: %d" roundSum


let parseLineAlt (line: string) =
    let theirs, desiredResult = getLineContents line
    let theirs = theirs |> Shape.create
    let desiredResult = desiredResult |> Round.create

    theirs, desiredResult

let runRoundAlt (line: string) =
    let line = line.Trim()
    let theirs, desiredResult = parseLineAlt line
    let mine = Round.resultShape theirs desiredResult

    (mine, theirs) ||> roundPointTotal


let allRoundsAlt = lines |> List.map runRoundAlt
let roundSumAlt = allRoundsAlt |> List.sum


printfn "Total round sum alt: %d" roundSumAlt
