open System
open System.IO

let lineToCrates (line: string) =
  line.ToCharArray()
  |> Array.chunkBySize 4
  |> Array.map (fun item -> item[1])
  |> Array.toList

let initStackCountOrAcc (length: int) (acc: char list list) =
  if acc.Length = 0 then List.init length (fun _ -> []) else acc

let drawingToStacks input =
  input
  |> Seq.fold (
    fun acc (line: string) ->
      let crates = lineToCrates line
      let initialised = initStackCountOrAcc crates.Length acc
      (crates, initialised) ||> List.map2 (fun c i -> if c = ' ' then i else List.append i [c])
    ) []

let drawingToMoves input =
  input
  |> Seq.fold(
    fun acc (line: string) ->
      let move =
        line.Split(" ")
        |> Array.filter (fun item -> item <> "move" && item <> "from" && item <> "to")
        |> fun arr -> (arr[0] |> int, arr[1] |> int, arr[2] |> int)
      (acc, [move]) ||> List.append
    ) []

let readFile path =
  try
    File.ReadLines(path)
    |> Ok
  with
  | ex -> Error ex

let moveCrates sDraw mDraw =
  let stacks = sDraw |> Result.map drawingToStacks
  let moves = mDraw |> Result.map drawingToMoves

  match (stacks, moves) with
  | (Ok stks, Ok mvs) -> 
    Ok (mvs
      |> List.fold (
        fun (acc: char list list) (move, from, too) ->
          let moveFrom = List.item (from - 1) acc
          let toMove = moveFrom[0..(move - 1)] // Use |> List.rev for part 1
          let reduced = moveFrom[move..(moveFrom.Length - 1)]
          let increased = [toMove; (List.item (too - 1) acc)] |> List.concat
          acc
          |> List.toArray
          |> fun arr ->
            Array.set arr (from - 1) reduced
            Array.set arr (too - 1) increased
            arr
          |> Array.toList
        ) stks
        |> List.map (fun l -> l[0]) 
        |> fun l -> String.Concat(Array.ofList(l)))
  | (_,_) -> Error "Something went wrong moving crates"

let stacksDrawing =
  Path.Combine(__SOURCE_DIRECTORY__, "resources", "crates-stack.txt")
  |> readFile

let movesDrawing =
  Path.Combine(__SOURCE_DIRECTORY__, "resources", "crates-move.txt")
  |> readFile

let solution = moveCrates stacksDrawing movesDrawing

printfn "The order of creates is %A" solution