#load "FileHelpers.fs"
#load "Helpers.fs"

open System
open System.Text.RegularExpressions
open Helpers


let fileName = $"5-ex"

let lines = FileHelpers.readNumFile fileName

let countIndex =
    lines
    |> List.findIndex (fun i -> i.StartsWith(" 1"))

let stacks, moves = lines |> List.splitAt (countIndex + 1)

let stacksRev = stacks |> List.rev
let (::) indexesLine, crateLines = stacksRev

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
    for i in indexValues do
        match currentLineCrateList.TryFind i with
        | None -> stackMap
        | Some crate ->
            match stackMap.TryFind i with
            | None -> stackMap.Add(i, [ crate ])
            | Some crateList ->
                stackMap.Add(i, crate::crateList)

let crateStacks = parsedCrates |> Seq.fold

printfn $"{line2}"
