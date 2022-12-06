open System.IO

exception RuckSackError of string

let readFile path =
  try
    File.ReadLines(path)
    |> Ok
  with
  | ex -> Error ex

let toCompartments (line: string) =
  let pivot = line.Length / 2
  (line[0..pivot - 1].ToCharArray(), line[pivot..line.Length].ToCharArray())

let findDuplicateItem (c1: char array, c2: char array): Result<char, string> =
  c1
  |> Array.filter (fun item -> Array.contains item c2)
  |> fun arr ->
    match arr with
    | arr when arr.Length >= 1 -> Ok arr[0]
    | _ -> Error "No duplicate item found"

let getItemPriority (item: char) =
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".ToCharArray()
  |> Array.tryFindIndex (fun elem -> elem = item)
  |> fun index ->
    match index with
    | Some i -> Some (i + 1)
    | None -> None

let implement list =
  list
  |> Seq.fold (
    fun acc line ->
      line
      |> toCompartments
      |> findDuplicateItem
      |> Result.map getItemPriority
      |> fun priority ->
        match priority with
        | Ok v -> 
          match v with
          | Some i -> acc + i
          | None -> raise (RuckSackError "No priority found")
        | Error ex -> raise (RuckSackError ex)
    ) 0

Path.Combine(__SOURCE_DIRECTORY__, "resources", "rucksacks.txt")
  |> readFile
  |> Result.map implement
  |> fun result ->
    match result with
    | Ok i -> printfn "The total priority of duplicate items is %i" i
    | Error ex -> printfn "%A" ex