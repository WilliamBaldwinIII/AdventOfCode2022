#load "FileHelpers.fs"
#load "Helpers.fs"

open Helpers
open System

fsi.ShowDeclarationValues <- false
fsi.ShowProperties <- false

let fileName = $"11"

let fileLines = FileHelpers.readNumFile fileName

type Monkey =
    { /// Monkey ID number in the list.
      Id: int

      /// Lists your worry level for each item the monkey is currently holding in the order they will be inspected.
      Items: int list

      /// Shows how your worry level changes as that monkey inspects an item.
      /// (An operation like new = old * 5 means that your worry level after the monkey inspected
      /// the item is five times whatever your worry level was before inspection.)
      Operation: int -> int

      /// Shows how the monkey uses your worry level to decide where to throw an item next.
      TestDivisibleBy: int


      /// Shows what happens with an item if the Test was true.
      /// Throw to the monkey with the given ID if true.
      ThrowToMonkeyIfTrue: int


      /// Shows what happens with an item if the Test was false.
      /// Throw to the monkey with the given ID if false.
      ThrowToMonkeyIfFalse: int }


module Monkey =
    let parse (lines: string list)
