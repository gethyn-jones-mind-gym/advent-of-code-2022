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

let decryptPlay c =
  match c with
  | 'A' -> Ok Rock
  | 'B' -> Ok Paper
  | 'C' -> Ok Scissors
  | _ -> Error "Problem decrypting play"

let decryptRoundEnding c =
  match c with
  | 'X' -> Ok Lose
  | 'Y' -> Ok Draw
  | 'Z' -> Ok Win
  | _ -> Error "Problem decrypting round end"

let decryptRound (line: string) =
  let decrypted = (decryptPlay line[0], decryptRoundEnding line[2])
  match decrypted with
  | ((Ok a), (Ok b)) -> Ok (a,b)
  | _ -> Error "Problem decrpyting round"

let determineOutcome ending =
  match ending with
  | Draw -> (Draw, Draw)
  | Win -> (Lose, Win)
  | Lose -> (Win, Lose)

let getShapeForEnding input =
  match input with
  | (Rock, Lose) -> Scissors
  | (Rock, Win) -> Paper
  | (Rock, Draw) -> Rock
  | (Paper, Lose) -> Rock
  | (Paper, Win) -> Scissors
  | (Paper, Draw) -> Paper
  | (Scissors, Lose) -> Paper
  | (Scissors, Win) -> Rock
  | (Scissors, Draw) -> Scissors

let calculateScoreForRound (shape, ending) =
  ending
  |> determineOutcome
  |> fun (a,b) -> (
    Outcome.score a + Shape.score shape,
    Outcome.score b + Shape.score (getShapeForEnding (shape, ending)))

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