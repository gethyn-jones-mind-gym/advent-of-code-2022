open System.IO

let readFile path =
  try
    File.ReadLines(path)
    |> Ok
  with
  | ex -> Error ex

let addLineToTally (maxCalories, tally) line =
  (maxCalories, tally + (line |> int))

let keepMaxAndResetTally (maxCalories, tally) =
  (max maxCalories tally, 0)

let (|IsEmptyLine|_|) (line: string) =
  if line.Length = 0 then Some () else None

let keepMaxCalories acc (line: string) =
  match line with
  | IsEmptyLine -> keepMaxAndResetTally acc
  | _ -> addLineToTally acc line

let calculateMaxCalories data =
  data
  |> Seq.fold keepMaxCalories (0,0)
  |> fun (maxCalories, _) -> maxCalories

Path.Combine(__SOURCE_DIRECTORY__, "resources", "calories.txt")
  |> readFile
  |> Result.map calculateMaxCalories
  |> fun result ->
    match result with
    | Ok i -> printfn "The elf with the most calories is carrying %i calories" i
    | Error ex -> printfn "%A" ex