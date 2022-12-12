open System.IO

let inline charToInt c = int c - int '0'

let to2dArray (input: string seq) =
  input
  |> Seq.map (fun line ->
    line.ToCharArray() 
    |> Array.map (fun c -> c |> charToInt))
  |> Seq.toArray
  |> fun arr -> array2D arr

let isTreeVisible trees tree =
  let taller = trees |> Array.tryFind (fun t -> t >= tree)
  match taller with
  | Some _ -> false
  | None -> true

let findVisibleTrees (trees: int[,]) =
  trees
  |> Array2D.mapi (fun i j tree ->
    let row = trees[i, *]
    let column = trees[*, j]
    let isTreeVisibleLeft = isTreeVisible row[..j-1] tree
    let isTreeVisibleRight = isTreeVisible row[j+1..] tree
    let isTreeVisibleUp = isTreeVisible column[..i-1] tree
    let isTreeVisibleDown = isTreeVisible column[i+1..] tree
    (tree, isTreeVisibleLeft || isTreeVisibleRight || isTreeVisibleUp || isTreeVisibleDown))

let countVisibleTrees (visible: (int * bool)[,]) =
  visible
  |> Seq.cast<(int * bool)>
  |> Seq.fold (fun acc (t,b) -> if b then acc + 1 else acc) 0

let getScenicScore (trees: int array) tree =
  trees
  |> Array.fold (
    fun (score, blocked) t -> 
      match (score, blocked) with
      | (_, true) -> (score, blocked)
      | (_, false) ->
        if t >= tree then (score + 1, true) else (score + 1, false)
    ) (0, false)
  |> fun (score, _) -> score

let getScenicScores (trees: int[,]) =
  trees
  |> Array2D.mapi (fun i j tree ->
    let row = trees[i, *]
    let column = trees[*, j]
    let right = row[j+1..]
    let left = row[..j-1] |> Array.rev
    let up = column[..i-1] |> Array.rev
    let down = column[i+1..]
    let scoreLeft = getScenicScore left tree
    let scoreRight = getScenicScore right tree
    let scoreUp = getScenicScore up tree
    let scoreDown = getScenicScore down tree
    scoreUp * scoreLeft * scoreRight * scoreDown)

let getHighestScore trees =
  trees
  |> Seq.cast<int>
  |> Seq.max

let readFile path =
  try
    File.ReadLines(path)
    |> Ok
  with
  | ex -> Error ex

let Part1 =
  Path.Combine(__SOURCE_DIRECTORY__, "resources", "tree-grid.txt")
  |> readFile
  |> Result.map to2dArray
  |> Result.map findVisibleTrees
  |> Result.map countVisibleTrees
  |> fun result ->
    match result with
    | Ok i -> printfn "The number if visible trees is %A" i
    | Error ex -> printfn "%A" ex

let Part2 =
  Path.Combine(__SOURCE_DIRECTORY__, "resources", "tree-grid.txt")
  |> readFile
  |> Result.map to2dArray
  |> Result.map getScenicScores
  |> Result.map getHighestScore
  |> fun result ->
    match result with
    | Ok i -> printfn "The highest scenic score is %A" i
    | Error ex -> printfn "%A" ex