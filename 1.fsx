#load "Helpers.fs"

let readFile() = Helpers.readFile @".\inputs\1.txt"

let lines = readFile()

let nums =
    lines
    |> List.map int

/// Get amount of times the number increased from one index to the next in the given list.
let getNumberOfIncreases (nums: int list) =
    let increases = seq {
        for i in 1 .. nums.Length - 1 do
            yield nums[i] > nums[i-1]
    }

    increases
    |> Seq.filter id
    |> Seq.length

let answer1 =
    nums
    |> getNumberOfIncreases

printfn $"Answer 1: {answer1}"

/// Get sums of sliding window of three indices.
let threeSlidingWindow = seq {
    for i in 2 .. nums.Length - 1 do
        yield nums[i - 2] + nums[i - 1] + nums[i]
}

let answer2 =
    threeSlidingWindow
    |> Seq.toList
    |> getNumberOfIncreases

printfn $"Answer 2: {answer2}"
