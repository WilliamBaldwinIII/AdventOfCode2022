#load "Helpers.fs"

open System

let fileName = $"1"

let readFile () =
    Helpers.readFile $".\inputs\{fileName}.txt"

let lines = readFile ()

let rec processFn (allElvesList: string list list) (currentElf: string list) (input: string list) =
    match input with
    | [] -> currentElf :: allElvesList
    | x :: xs ->
        if x |> String.IsNullOrWhiteSpace then
            processFn (currentElf :: allElvesList) [] xs
        else
            let currentElf = x :: currentElf
            processFn allElvesList currentElf xs

let elfTotalsRaw = lines |> processFn [] []
lines |> List.fold

let elfTotals = elfTotalsRaw |> List.map (List.sumBy int)

printfn "Elf total calories: %A" elfTotals
printfn $"Max calorie count: {elfTotals |> List.max}"


let topThreeCalories =
    elfTotals
    |> List.sortByDescending id
    |> List.take 3
    |> List.sum


printfn "Top 3 sum: %d" topThreeCalories
