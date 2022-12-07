#load "FileHelpers.fs"
#load "Helpers.fs"

open System
open System.Text.RegularExpressions
open Helpers


let fileName = $"7"

let lines = FileHelpers.readNumFile fileName

type Command =
    | List
    | ChangeDir of string

type File = { Name: string; Size: bigint }

type Directory =
    { Name: string
      Files: File list
      Directories: Directory list }
//and FileType
