module Helpers

open System.IO
open System

let readFile path = File.ReadAllLines(path) |> Array.toList
let readNumFile fileName = readFile $".\inputs\{fileName}.txt"

let readFileArr path = File.ReadAllLines(path)

let loop n fn starter =
    let rec doIt curIndex intermediate =
        if curIndex >= n then
            intermediate
        else
            let newIntermediate = intermediate |> fn
            doIt (curIndex + 1) newIntermediate

    doIt 0 starter


module Array2D =
    // Thank you, https://stackoverflow.com/a/49891028 !!!
    let tryFindIndex item (arr: 'a [,]) =
        let rec go x y =
            if y >= arr.GetLength 1 then
                None
            elif x >= arr.GetLength 0 then
                go 0 (y + 1)
            elif arr.[x, y] = item then
                Some(x, y)
            else
                go (x + 1) y

        go 0 0

    // https://stackoverflow.com/a/2369690
    let flatten (A: 'a [,]) = A |> Seq.cast<'a>

    // https://stackoverflow.com/a/2369690
    let getColumn c (A: 'a [,]) = flatten A[*, c..c] |> Seq.toArray

    // https://stackoverflow.com/a/2369690
    let getRow r (A: 'a [,]) = flatten A[r..r, *] |> Seq.toArray


module String =
    let split (delimeter: string) (str: string) =
        str.Split(delimeter, StringSplitOptions.RemoveEmptyEntries)

    let fromCharArray (cArr: char array) = new string (cArr)
