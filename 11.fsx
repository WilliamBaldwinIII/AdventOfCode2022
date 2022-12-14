open System.Text.RegularExpressions


#load "FileHelpers.fs"
#load "Helpers.fs"

open Helpers
open System

//fsi.ShowDeclarationValues <- false
//fsi.ShowProperties <- false

let fileName = $"11"

let fileLines = FileHelpers.readNumFile' fileName

type Item = int
type MonkeyId = int

type Monkey =
    { /// Monkey ID number in the list.
      Id: MonkeyId

      /// Lists your worry level for each item the monkey is currently holding in the order they will be inspected.
      Items: Item list

      /// Shows how your worry level changes as that monkey inspects an item.
      /// (An operation like new = old * 5 means that your worry level after the monkey inspected
      /// the item is five times whatever your worry level was before inspection.)
      Operation: Item -> Item

      /// Shows how the monkey uses your worry level to decide where to throw an item next.
      TestDivisibleBy: int


      /// Shows what happens with an item if the Test was true.
      /// Throw to the monkey with the given ID if true.
      ThrowToMonkeyIfTrue: MonkeyId


      /// Shows what happens with an item if the Test was false.
      /// Throw to the monkey with the given ID if false.
      ThrowToMonkeyIfFalse: MonkeyId


      /// How many items has this monkey examined in the given timeframe?
      NumItemsInspected: int }


module Monkey =
    type private Operand =
        | Number of int
        | Old

    let private monkeyIdRegex = Regex(@"Monkey (\d+):", RegexOptions.Compiled)

    let private parseMonkeyId (line: string) =
        if monkeyIdRegex.IsMatch line then
            monkeyIdRegex.Match(line).Groups[1].Value |> int
        else
            failwith "Invalid Monkey ID syntax"

    let private parseStartItems (line: string) =
        let substr = line |> String.afterString "Starting items: "
        let items = substr |> String.split ","

        items |> List.ofArray |> List.map int

    let private parseOperation (line: string) =

        let parseOperator =
            function
            | "*" -> (*)
            | "/" -> (/)
            | "+" -> (+)
            | "-" -> (-)
            | other -> failwith $"{other} is an invalid operator!"

        let parseOperand (str: String) =
            let isNumber, number = Int32.TryParse str

            if isNumber then
                Number number
            elif str = "old" then
                Old
            else
                failwith $"{str} is not a valid operand!"

        let substr = line |> String.afterString "Operation: new = "
        let items = substr |> String.split " "

        match items with
        | [| beforeOperand; operator; afterOperand |] ->
            let beforeOperand' = parseOperand beforeOperand
            let afterOperand' = parseOperand afterOperand
            let operator' = parseOperator operator

            match beforeOperand', afterOperand' with
            | Number before, Number after -> (fun _ -> operator' before after)
            | Number before, Old -> (fun old -> operator' before old)
            | Old, Number after -> (fun old -> operator' old after)
            | Old, Old -> (fun old -> operator' old old)

        | i -> failwith $"Invalid operation! %A{i}"

    let private parseTestDivisible = String.afterString "Test: divisible by " >> int

    let private parseTrueMonkey =
        String.afterString "If true: throw to monkey "
        >> int

    let private parseFalseMonkey =
        String.afterString "If false: throw to monkey "
        >> int

    let parse (lines: string list) =
        match lines with
        | [ idLine; itemsLines; operationLine; testDivisibleLine; ifTrueLine; ifFalseLine ] ->
            let monkeyId = parseMonkeyId idLine
            let startingItems = parseStartItems itemsLines
            let operation = parseOperation operationLine
            let testDivisibleBy = parseTestDivisible testDivisibleLine
            let monkeyIdIfTrue = parseTrueMonkey ifTrueLine
            let monkeyIdIfFalse = parseFalseMonkey ifFalseLine

            { Id = monkeyId
              Items = startingItems
              Operation = operation
              TestDivisibleBy = testDivisibleBy
              ThrowToMonkeyIfTrue = monkeyIdIfTrue
              ThrowToMonkeyIfFalse = monkeyIdIfFalse
              NumItemsInspected = 0 }

        | _ -> failwith $"Invalid number of lines! %A{lines}"

    let makeMove (monkeyMap: Map<MonkeyId, Monkey>) (monkeyId: MonkeyId) =
        let monkey: Monkey = monkeyMap[monkeyId]

        let throwItem (monkeyMap: Map<MonkeyId, Monkey>) (throwToMonkeyId: MonkeyId) (item: Item) =
            let throwToMonkey = monkeyMap[throwToMonkeyId]
            let oldItems = throwToMonkey.Items
            let newItems = oldItems @ [ item ]

            monkeyMap.Add(throwToMonkeyId, { throwToMonkey with Items = newItems })

        let handleItem (monkeyMap: Map<MonkeyId, Monkey>) (item: Item) =
            let itemNewWorryLevel = monkey.Operation item
            let itemNewWorryLevel = itemNewWorryLevel / 3

            let throwToMonkeyId =
                if itemNewWorryLevel % monkey.TestDivisibleBy = 0 then
                    monkey.ThrowToMonkeyIfTrue
                else
                    monkey.ThrowToMonkeyIfFalse

            itemNewWorryLevel
            |> throwItem monkeyMap throwToMonkeyId

        let itemCount = monkey.Items |> List.length
        let newMonkeyMap = monkey.Items |> List.fold handleItem monkeyMap

        // Monkey has inspected and thrown all its items
        let newMonkey =
            { monkey with
                Items = []
                NumItemsInspected = monkey.NumItemsInspected + itemCount }

        newMonkeyMap.Add(monkey.Id, newMonkey)

//let chunked =
//    fileLines
//    |> List.filter (String.IsNullOrWhiteSpace >> not)
//    |> List.chunkBySize 6
//    |> List.mapi (fun i chunk -> i, chunk)
//    |> List.iter (fun (i, chunk) ->
//        chunk
//        |> List.iter (fun line -> Console.WriteLine($"{i}: {line}")))

let runRound (monkeyMap: Map<MonkeyId, Monkey>) =
    monkeyMap.Values
    |> Seq.map (fun monkey -> monkey.Id)
    |> Seq.fold Monkey.makeMove monkeyMap

let runRoundXTimes x (monkeyMap: Map<MonkeyId, Monkey>) =
    [ 1..x ]
    |> List.fold (fun monkeyMap _ -> runRound monkeyMap) monkeyMap

let monkeys =
    fileLines
    |> List.filter (String.IsNullOrWhiteSpace >> not)
    |> List.chunkBySize 6
    |> List.map Monkey.parse

let monkeyMap =
    monkeys
    |> List.map (fun m -> m.Id, m)
    |> Map.ofList

let newMonkeyMap = monkeyMap |> runRoundXTimes 20

let top2 =
    newMonkeyMap.Values
    |> Seq.sortByDescending (fun monkey -> monkey.NumItemsInspected)
    |> Seq.take 2
    |> Seq.toList

let top2Product =
    top2
    |> List.map (fun m -> m.NumItemsInspected)
    |> List.product
