#load "FileHelpers.fs"
#load "Helpers.fs"

open System
open Helpers


let fileName = $"4-ex"

let lines = FileHelpers.readNumFile fileName


let splitInTwo line =
    let split = line |> String.split ","
    split[0], split[1]

let parseRange rangeStr =
    let split = rangeStr |> String.split "-"
    let num1 = split[0] |> int
    let num2 = split[1] |> int

    [ num1..num2 ] |> set

let isSubset (set1, set2) =
    let intersection = Set.intersect set1 set2

    intersection = set1 || intersection = set2

let isOverlap (set1, set2) =
    let intersection = Set.intersect set1 set2

    intersection |> Seq.isEmpty |> not

let rangePairs =
    lines
    |> List.map splitInTwo
    |> List.map (fun (elf1, elf2) -> parseRange elf1, parseRange elf2)

let subsetPairs = rangePairs |> List.filter isSubset

let numSubsetPairs = subsetPairs |> List.length
printfn $"(Part 1) Number of subset pairs: %d{numSubsetPairs}"

let overlapPairs = rangePairs |> List.filter isOverlap
let numoverlapPairs = overlapPairs |> List.length

printfn $"(Part 2) Number of overlap pairs: %d{numoverlapPairs}"
