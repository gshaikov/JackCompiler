module Jack.UseCases

open Jack
open Compositions

let translateInstruction labelStream vmFileName instruction =
    match VirtualMachine.parse vmFileName instruction with
    | VirtualMachine.POk instruction ->
        instruction
        |> Translator.translate (labelStream ())
        |> Assembly.serialiseList
        |> Ok
    | VirtualMachine.PNone -> Ok("")
    | VirtualMachine.PError e -> Error e
