open System.IO

exception AssignmentsError of string

let splitAssignments (s: string) =
  let arr = s.Split(",")
  match arr.Length with
  | 2 -> Ok (arr[0], arr[1])
  | l when l < 2 -> Error "Not enough assignments"
  | _ -> Error "Too many assignments"

let toSingleDigitSections (shorthand: string) =
  let arr = shorthand.Split("-")
  [arr[0] |> int .. arr[1] |> int]

let toSections assignments =
  assignments
  |> fun (a1, a2) -> (a1 |> toSingleDigitSections, a2 |> toSingleDigitSections)

let overlaps (a, b) =
  a
  |> List.except b
  |> fun l -> l.Length < a.Length

let implement lines =
  try
    lines
    |> Seq.fold
      (fun acc line ->
        line
        |> splitAssignments
        |> Result.map toSections
        |> Result.map overlaps
        |> fun b ->
            match b with
            | Ok b -> if b = true then acc + 1 else acc
            | Error ex -> raise (AssignmentsError ex)
      ) 0
      |> Ok
  with
    | :? AssignmentsError as ex -> Error ex

let readFile path =
  try
    File.ReadLines(path)
    |> Ok
  with
  | ex -> Error ex

Path.Combine(__SOURCE_DIRECTORY__, "resources", "camp-cleanup.txt")
  |> readFile
  |> Result.map implement
  |> Result.map
    (fun result ->
      match result with
      | Ok i -> printfn "The total assignments where one range fully contains the other is %A" i
      | Error ex -> printfn "%A" ex)