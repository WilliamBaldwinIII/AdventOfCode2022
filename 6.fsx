#load "FileHelpers.fs"
#load "Helpers.fs"

open System
open System.Text.RegularExpressions
open Helpers


let fileName = $"6-ex"

let lines = FileHelpers.readNumFile fileName

let oneLine = lines |> Seq.exactlyOne

let chars = oneLine.ToCharArray()


/// Gets a window of X elements in the array
let getWindowOfX x i (arr: _ array) =
    let x = x - 1
    let endIndex = i + x

    seq {
        for i' in i..endIndex do
            yield arr[i']
    }

let isMarker x i (arr: _ array) =
    let window = arr |> getWindowOfX x i
    let windowSet = window |> set

    if window |> Seq.length = windowSet.Count then
        true
    else
        false

let packetNum = 4
let messageNum = 14
let isStartOfPacketMarker = isMarker packetNum
let isStartOfMessageMarker = isMarker messageNum

let indexedChars = chars |> Array.indexed

let firstPacketIndex =
    indexedChars
    |> Array.findIndex (fun (i, _) -> isStartOfPacketMarker i chars)
    |> (+) packetNum

let firstMessageIndex =
    indexedChars
    |> Array.findIndex (fun (i, _) -> isStartOfMessageMarker i chars)
    |> (+) messageNum
