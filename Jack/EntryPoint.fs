// Learn more about F# at http://fsharp.org

open System

open Jack

let createLabelStream baseLabel =
    let mutable labelSuffix = 0

    let inner () =
        labelSuffix <- labelSuffix + 1
        sprintf "%s_%i" baseLabel labelSuffix

    inner

let translateFile (vmFilePath: string) outputFileName =
    let labelStream = createLabelStream "__AUTO_LABEL"
    let translator = UseCases.translateInstruction labelStream (IO.Path.GetFileName vmFilePath)
    FileIO.mapFile translator vmFilePath outputFileName

let run argv =
    if Array.length argv = 2
    then translateFile argv.[0] argv.[1]
    else Error "Usage: dotnet run vmFileName outputFileName"

let handleExit res =
    match res with
    | Ok _ -> 0
    | Error e -> printfn "%s" e; 1

[<EntryPoint>]
let main argv =
    run argv |> handleExit
