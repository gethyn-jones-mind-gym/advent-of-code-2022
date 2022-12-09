open System.IO

exception CommunicationSystemError of string

let uniqueChars (arr: char array) =
  (Set arr).Count = arr.Length

let getMarkerEnd (markerSize) (data: string seq) =
  data
  |> Seq.head
  |> fun datastream -> datastream.ToCharArray()
  |> Array.windowed markerSize
  |> Array.tryFindIndex uniqueChars
  |> fun index ->
    match index with
    | Some i -> Ok (i + markerSize)
    | None -> Error (CommunicationSystemError "No marker found")
  
let getStartOfPacketMarkerEnd = getMarkerEnd 4

let getStartOfMessageMarkerEnd = getMarkerEnd 14

let readFile path =
  try
    File.ReadLines(path)
    |> Ok
  with
  | ex -> Error ex

let Part1 =
  Path.Combine(__SOURCE_DIRECTORY__, "resources", "buffer.txt")
  |> readFile
  |> Result.bind getStartOfPacketMarkerEnd
  |> fun result ->
    match result with
    | Ok i -> printfn "The first start of packet marker is %A" i
    | Error ex -> printfn "%A" ex

let Part2 =
  Path.Combine(__SOURCE_DIRECTORY__, "resources", "buffer.txt")
  |> readFile
  |> Result.bind getStartOfMessageMarkerEnd
  |> fun result ->
    match result with
    | Ok i -> printfn "The first start of message marker is %A" i
    | Error ex -> printfn "%A" ex