module Jack.FileIO

open System.IO
open Jack.Compositions

let rec tryIter f x =
    match Seq.tryHead x with
    | Some item ->
        match f item with
        | Ok _ -> Seq.tail x |> tryIter f
        | Error e -> Error e
    | None -> Ok()

// https://stackoverflow.com/questions/2365527/how-read-a-file-into-a-seq-of-lines-in-f
let createReader (file: string) =
    seq {
        use sr = new StreamReader(file)
        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

let writeString (outputStream: StreamWriter) =
    tryCatch (fun (line: string) -> outputStream.WriteLine("{0}", line)) (fun ex -> ex.Message)

let writeResult outputStream = writeString outputStream |> bind

let writeToFile file assemblyResults =
    try
        using (File.CreateText(file)) (fun outputStream -> tryIter (writeResult outputStream) assemblyResults)
    with ex -> Error ex.Message

let mapFile transformFunc inputFile outputFile =
    createReader inputFile
    |> Seq.map transformFunc
    |> writeToFile outputFile
