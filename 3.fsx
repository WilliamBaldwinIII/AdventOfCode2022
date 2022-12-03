#load "FileHelpers.fs"

open System

let fileName = $"3"

let lines = FileHelpers.readNumFile fileName

printfn $"{int 'a'}"
printfn $"{int 'p'}"
printfn $"{int 'A'}"

let splitIntoCompartments (line: string) =
    let half = line.Length / 2
    let first = line.Substring(0, half)
    let second = line.Substring(half, half)

    first.ToCharArray(), second.ToCharArray()


let findCommonCharacter (first, second) =
    let firstSet = first |> Set.ofSeq
    let secondSet = second |> Set.ofSeq

    let difference = Set.intersect firstSet secondSet
    difference |> Seq.exactlyOne

let findCommonCharacter' allSacks =
    allSacks
    |> Seq.map set
    |> Set.intersectMany
    |> Seq.exactlyOne

let mapToPriority (c: char) =
    let cInt = int c

    if c |> Char.IsUpper then
        // A = 65
        (cInt - 65) + 1 + 26
    elif c |> Char.IsLower then
        // a = 97
        (cInt - 97) + 1
    else
        failwith $"{c} is not a valid character"

let prioritySum =
    lines
    |> List.map (
        splitIntoCompartments
        >> findCommonCharacter
        >> mapToPriority
    )
    |> List.sum

printfn $"Priority Sum (Part 1): %d{prioritySum}"


let elfGroups = lines |> List.chunkBySize 3

let badgeSum =
    elfGroups
    |> List.map (findCommonCharacter' >> mapToPriority)
    |> List.sum

printfn $"Badge Sum (Part 2): %d{badgeSum}"
