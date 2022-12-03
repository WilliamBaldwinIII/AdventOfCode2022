module FileHelpers

open System.IO
open System

let readFile path = File.ReadAllLines(path) |> Array.toList
let readNumFile fileName = readFile $".\inputs\{fileName}.txt"

let readFileArr path = File.ReadAllLines(path)
