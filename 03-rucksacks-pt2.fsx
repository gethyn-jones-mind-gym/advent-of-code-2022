open System.IO

exception RuckSackError of string

let readFile path =
  try
    File.ReadLines(path)
    |> Ok
  with
  | ex -> Error ex

let findCommonItem (lines: string array) =
  lines
  |> Array.map (fun l -> l |> fun m -> m.ToCharArray() |> Set.ofArray)
  |> Set.intersectMany
  |> Set.toArray
  |> fun arr ->
    match arr with
    | [|a|] -> Some a
    | _ -> None

let getItemPriority (item: char) =
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".ToCharArray()
  |> Array.tryFindIndex (fun elem -> elem = item)
  |> fun index ->
    match index with
    | Some i -> Some (i + 1)
    | None -> None

let implement list =
  list
  |> Seq.chunkBySize 3
  |> Seq.fold (
    fun acc lines ->
      lines
      |> findCommonItem
      |> Option.map getItemPriority
      |> fun priority ->
        // TODO figure out how to unwrap nested option
        match priority with
        | Some v -> 
          match v with
          | Some i -> acc + i
          | None -> raise (RuckSackError "No priority found")
        | None -> raise (RuckSackError "No priority found")
    ) 0

Path.Combine(__SOURCE_DIRECTORY__, "resources", "rucksacks.txt")
  |> readFile
  |> Result.map implement
  |> fun result ->
    match result with
    | Ok i -> printfn "The total priority of common items is %i" i
    | Error ex -> printfn "%A" ex