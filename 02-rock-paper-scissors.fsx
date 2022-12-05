open System.IO

type Shape =
| Rock
| Paper
| Scissors

type Outcome =
| Win
| Lose
| Draw

exception StrategyCalculationError of string

module Shape =
  let score shape =
    match shape with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

module Outcome =
  let score score =
    match score with
    | Win -> 6
    | Lose -> 0
    | Draw -> 3

let readFile path =
  try
    File.ReadLines(path)
    |> Ok
  with
  | ex -> Error ex

let determineOutcome (a, b) =
  match (a, b) with
  | (Rock, Rock) -> (Draw, Draw)
  | (Rock, Paper) -> (Lose, Win)
  | (Rock, Scissors) -> (Win, Lose)
  | (Paper, Rock) -> (Win, Lose)
  | (Paper, Paper) -> (Draw, Draw)
  | (Paper, Scissors) -> (Lose, Win)
  | (Scissors, Rock) -> (Lose, Win)
  | (Scissors, Paper) -> (Win, Lose)
  | (Scissors, Scissors) -> (Draw, Draw)

let toPlayerScoreBreakdown (shape, outcome) =
  (Shape.score shape, Outcome.score outcome)

let toRoundScoreBreakdown (playerA, playerB) =
  (toPlayerScoreBreakdown playerA, toPlayerScoreBreakdown playerB)

let toPlayerTotalScore (shapeScore, outcomeScore) =
  shapeScore + outcomeScore

let toTotalScore (breakdownA, breakdownB) =
  (toPlayerTotalScore breakdownA, toPlayerTotalScore breakdownB)

let calculateScoreForRound (shapeA, shapeB) =
  (shapeA, shapeB)
  |> determineOutcome
  |> fun (outcomeA, outcomeB) -> ((shapeA, outcomeA), (shapeB, outcomeB))
  |> toRoundScoreBreakdown
  |> toTotalScore

let decryptPlay c =
  match c with
  | 'A' -> Ok Rock
  | 'B' -> Ok Paper
  | 'C' -> Ok Scissors
  | 'X' -> Ok Rock
  | 'Y' -> Ok Paper
  | 'Z' -> Ok Scissors
  | _ -> Error "Problem decrypting play"

let decryptRound (line: string) =
  let decrypted = (decryptPlay line[0], decryptPlay line[2])
  match decrypted with
  | ((Ok a), (Ok b)) -> Ok (a,b)
  | _ -> Error "Problem decrpyting round"

let decryptAndCalculateRound line =
  line
  |> decryptRound
  |> Result.map calculateScoreForRound

let implementStrategy encrypted =
  encrypted
  |> Seq.fold (
      fun (a,b) line ->
        line
        |> decryptAndCalculateRound
        |> fun result ->
            match result with
            | Ok (nextA,nextB) -> (a + nextA, b + nextB)
            | Error ex -> raise (StrategyCalculationError ex)
    ) (0,0)

Path.Combine(__SOURCE_DIRECTORY__, "resources", "rock-paper-scissors-strategy.txt")
  |> readFile
  |> Result.map implementStrategy
  |> fun result ->
    match result with
    | Ok i -> printfn "The result of implementing the rock, paper, scissors strategy is %A" i
    | Error ex -> printfn "%A" ex

// let round1 = calculateScoreForRound (Rock, Paper)
// let round2 = calculateScoreForRound (Paper, Rock)
// let round3 = calculateScoreForRound (Scissors, Scissors)

// let rounds = [|(Rock, Paper);(Paper, Rock);(Scissors, Scissors)|]

// let assertRound1 = round1 = (1, 8)
// let assertRound2 = round2 = (8, 1)
// let assertRound3 = round3 = (6, 6)

// let assertRounds = calculateTotalForAllRounds rounds = (15, 15)

