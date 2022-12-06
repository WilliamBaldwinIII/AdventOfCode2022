#load "FileHelpers.fs"
#load "Helpers.fs"

open System
open System.Text.RegularExpressions
open Helpers


let fileName = $"5-ex"

let lines =
    FileHelpers.readNumFile fileName
    |> List.map (fun s -> s.Trim())

let countIndex =
    lines
    |> List.findIndex (fun i -> i.StartsWith("1"))

let stacks, moves' = lines |> List.splitAt (countIndex + 1)
let moves = moves' |> List.tail

let stacksRev = stacks |> List.rev

let indexesLine, crateLines =
    match stacksRev with
    | indexesLine :: crateLines -> indexesLine, crateLines
    | _ -> failwith "Error! Something's wrong with the parsed stack lines"

/// Map indexes to their programmatic indexes
let indexMap =
    indexesLine
    |> String.split " "
    |> Seq.map int
    |> Seq.mapi (fun i index -> index, i)
    |> Map.ofSeq

let crateRegex = Regex(@"\[([A-Z])\]", RegexOptions.Compiled)

let parseCrate (crate: string) =
    match crate.Trim() with
    | "" -> None
    | s when crateRegex.IsMatch s -> crateRegex.Match(s).Groups[1].Value |> Some
    | s -> $"Invalid crate syntax! {s}" |> Some

let parseLine (line: string) =
    line
    |> Seq.chunkBySize 4
    |> Seq.map String.fromCharArray
    |> Seq.map parseCrate
    |> Seq.mapi (fun i crate -> i, crate)
    |> Seq.choose (fun (i, crate) -> crate |> Option.map (fun c -> i, c))
    |> Map.ofSeq

let parsedCrates = crateLines |> Seq.map parseLine

let indexValues = indexMap.Values |> seq

let addLineToStacks (stackMap: Map<int, string list>) (currentLineCrateList: Map<int, string>) =
    let lineFold (stackMap': Map<int, string list>) i =
        match currentLineCrateList.TryFind i with
        | None -> stackMap'
        | Some crate ->
            match stackMap'.TryFind i with
            | None -> stackMap'.Add(i, [ crate ])
            | Some crateList -> stackMap'.Add(i, crate :: crateList)

    indexValues |> Seq.fold lineFold stackMap


let convertToMapIndex puzzleIndex = indexMap |> Map.find puzzleIndex

let moveRegex = Regex(@"move (\d+) from (\d+) to (\d+)", RegexOptions.Compiled)

let parseMove (line: string) =
    if not <| moveRegex.IsMatch line then
        failwith $"Not a valid move line! {line}"

    let moveMatchGroups = moveRegex.Match(line).Groups
    let amountToMove = moveMatchGroups[1].Value |> int
    let fromStackIndex = moveMatchGroups[2].Value |> int
    let toStackIndex = moveMatchGroups[3].Value |> int

    let fromStack = convertToMapIndex fromStackIndex
    let toStack = convertToMapIndex toStackIndex

    amountToMove, fromStack, toStack

let makeMove (crateStacks: Map<int, string list>) (line: string) =
    let amountToMove, fromStackNum, toStackNum = parseMove line

    let fromStack = crateStacks[fromStackNum]
    let toStack = crateStacks[toStackNum]

    let cratesToAdd = fromStack |> List.take amountToMove
    let fromStack' = fromStack |> List.skip amountToMove
    let toStack' = (cratesToAdd |> List.rev) @ toStack

    crateStacks
    |> Map.add fromStackNum fromStack'
    |> Map.add toStackNum toStack'


let initialCrateStacks = parsedCrates |> Seq.fold addLineToStacks Map.empty

printfn $"Moves: {moves}"
let afterMoveCrateStacks = moves |> Seq.fold makeMove initialCrateStacks

printfn $"After Moves: %A{afterMoveCrateStacks}"
