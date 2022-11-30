open System
open System.IO
open System.Diagnostics

open Helpers

let fileName = $"1"

let fileLines =
    $"..\..\..\inputs\{fileName}.txt"
    |> File.ReadAllLines
    |> List.ofArray
