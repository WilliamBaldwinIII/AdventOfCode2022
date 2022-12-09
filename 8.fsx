#load "FileHelpers.fs"
#load "Helpers.fs"

open System
open System.Text.RegularExpressions
open Helpers


let fileName = $"8-ex"

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
