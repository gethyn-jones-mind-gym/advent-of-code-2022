open System.IO

let readFile path =
  try
    File.ReadLines(path)
    |> Ok
  with
  | ex -> Error ex

let addLineToTally (maxCalories, tally) line =
  (maxCalories, tally + (line |> int))

let keepMaxThree ((max1, max2, max3), tally) =
  [|max1; max2; max3; tally|]
  |> Array.sort
  |> fun arr -> (arr.[1],arr.[2],arr.[3])
  
let keepMaxAndResetTally (maxCalories, tally) =
  (keepMaxThree (maxCalories, tally), 0)

let (|IsEmptyLine|_|) (line: string) =
  if line.Length = 0 then Some () else None

let keepMaxCalories acc (line: string) =
  match line with
  | IsEmptyLine -> keepMaxAndResetTally acc
  | _ -> addLineToTally acc line

let calculateMaxCalories data =
  data
  |> Seq.fold keepMaxCalories ((0,0,0), 0)
  |> fun ((max1, max2, max3), _) -> [|max1; max2; max3|] |> Array.sum

Path.Combine(__SOURCE_DIRECTORY__, "resources", "calories.txt")
  |> readFile
  |> Result.map calculateMaxCalories
  |> fun result ->
    match result with
    | Ok i -> printfn "The three elves carrying the most calories are carrying %A calories in total" i
    | Error ex -> printfn "%A" ex