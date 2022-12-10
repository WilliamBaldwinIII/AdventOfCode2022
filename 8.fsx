#load "FileHelpers.fs"
#load "Helpers.fs"

open Helpers


let fileName = $"8"

let fileLines = FileHelpers.readNumFile fileName

let parseLine (str: string) =
    str.ToCharArray()
    |> List.ofArray
    |> List.map (string >> int)

let isEdge (grid: _ [,]) (x, y) =
    x = 0
    || y = 0
    || x = (grid |> Array2D.length1) - 1
    || y = (grid |> Array2D.length2) - 1

let isVisibleFromEdge (grid: int [,]) (x, y) =
    if isEdge grid (x, y) then
        true
    else
        let currentElement = grid[x, y]

        let isVisible columnOrRow curIndex =
            let mapped = columnOrRow |> Seq.mapi (fun i e -> (i, e))

            let areAllLess = Seq.forall (fun (_, e) -> e < currentElement)

            let visibleFromBefore =
                mapped
                |> Seq.filter (fun (i, _) -> i < curIndex)
                |> areAllLess

            let visibleFromAfter =
                mapped
                |> Seq.filter (fun (i, _) -> i > curIndex)
                |> areAllLess

            visibleFromBefore || visibleFromAfter

        let column = grid |> Helpers.Array2D.getColumn y
        let row = grid |> Helpers.Array2D.getRow x

        let isVisibleVertical = isVisible column x
        let isVisibleHorizontal = isVisible row y

        isVisibleVertical || isVisibleHorizontal


let mainGrid = fileLines |> List.map parseLine |> array2D

let isVisibleFromEdge' x y _cur = isVisibleFromEdge mainGrid (x, y)

let allVisible =
    mainGrid
    |> Array2D.mapi isVisibleFromEdge'
    |> Array2D.flatten
    |> Seq.filter id
    |> Seq.length

// -------------- Part 2 --------------
let getVisibilityScore (grid: int [,]) (x, y) =
    let currentElement = grid[x, y]

    let getSection columnOrRow curIndex =
        let mapped = columnOrRow |> Seq.mapi (fun i e -> (i, e))

        let filter fn =
            mapped
            |> Seq.filter (fun (i, _) -> i |> fn)
            |> Seq.map snd
            |> Seq.toList

        let isBefore i = i < curIndex
        let isAfter i = i > curIndex

        let before = filter isBefore |> List.rev
        let after = filter isAfter

        [ before; after ]


    let column = grid |> Helpers.Array2D.getColumn y
    let row = grid |> Helpers.Array2D.getRow x

    let columnSections = getSection column x
    let rowSections = getSection row y

    let sumSection =
        function
        | [] -> 0
        | xs ->
            let numTrees =
                xs
                |> Seq.takeWhile (fun e -> e < currentElement)
                |> Seq.length

            // If our view goes all the way to the edge, the number of trees
            // is what we counted. Otherwise, add 1 to offset the taller tree we ran into
            if xs.Length = numTrees then
                numTrees
            else
                numTrees + 1

    let sections = [ columnSections; rowSections ] |> List.concat

    let product = sections |> List.map sumSection |> List.product
    product


let getVisibilityScore' x y _cur = getVisibilityScore mainGrid (x, y)

let allScores =
    mainGrid
    |> Array2D.mapi getVisibilityScore'
    |> Array2D.flatten
    |> Seq.max
