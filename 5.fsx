#load "FileHelpers.fs"
#load "Helpers.fs"

open System
open System.Text.RegularExpressions
open Helpers


let fileName = $"5"

let lines = FileHelpers.readNumFile fileName

let countIndex =
    lines
    |> List.findIndex (fun i -> i.StartsWith(" 1"))

let stacks, moves' = lines |> List.splitAt (countIndex + 1)
let moves = moves' |> List.tail |> List.map String.trim

let stacksRev = stacks |> List.rev

let indexesLine, crateLines =
    match stacksRev with
    | indexesLine :: crateLines -> indexesLine, crateLines
    | _ -> failwith "Error! Something's wrong with the parsed stack lines"

let indexes = indexesLine |> String.split " " |> Seq.map int

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
    |> Seq.choose (fun (i, crate) -> crate |> Option.map (fun c -> i + 1, c))
    |> Map.ofSeq

let parsedCrates = crateLines |> Seq.map parseLine


let addLineToStacks (stackMap: Map<int, string list>) (currentLineCrateList: Map<int, string>) =
    let lineFold (stackMap': Map<int, string list>) i =
        match currentLineCrateList.TryFind i with
        | None -> stackMap'
        | Some crate ->
            match stackMap'.TryFind i with
            | None -> stackMap'.Add(i, [ crate ])
            | Some crateList -> stackMap'.Add(i, crate :: crateList)

    indexes |> Seq.fold lineFold stackMap



let moveRegex = Regex(@"move (\d+) from (\d+) to (\d+)", RegexOptions.Compiled)

let parseMove (line: string) =
    if not <| moveRegex.IsMatch line then
        failwith $"Not a valid move line! {line}"

    let moveMatchGroups = moveRegex.Match(line).Groups
    let amountToMove = moveMatchGroups[1].Value |> int
    let fromStack = moveMatchGroups[2].Value |> int
    let toStack = moveMatchGroups[3].Value |> int

    amountToMove, fromStack, toStack

let makeMove reverse (crateStacks: Map<int, string list>) (line: string) =
    let amountToMove, fromStackNum, toStackNum = parseMove line

    let fromStack = crateStacks[fromStackNum]
    let toStack = crateStacks[toStackNum]

    let revFn = if reverse then List.rev else id

    let cratesToAdd = fromStack |> List.take amountToMove
    let fromStack' = fromStack |> List.skip amountToMove
    let toStack' = (cratesToAdd |> revFn) @ toStack

    crateStacks
    |> Map.add fromStackNum fromStack'
    |> Map.add toStackNum toStack'


let initialCrateStacks = parsedCrates |> Seq.fold addLineToStacks Map.empty

printfn $"Moves: {moves}"

let afterMoveCrateStacks =
    moves
    |> Seq.fold (makeMove true) initialCrateStacks

printfn $"After Moves: %A{afterMoveCrateStacks}"

let topCrates =
    afterMoveCrateStacks
    |> Map.values
    |> Seq.map List.head
    |> String.concat ""


let afterMoveCrateStacks' =
    moves
    |> Seq.fold (makeMove false) initialCrateStacks

let topCrates' =
    afterMoveCrateStacks'
    |> Map.values
    |> Seq.map List.head
    |> String.concat ""
