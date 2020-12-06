module Jack.UseCases

open Jack.Compositions

let createLabelStream baseLabel =
    let mutable labelSuffix = 0

    let inner () =
        labelSuffix <- labelSuffix + 1
        sprintf "%s_%i" baseLabel labelSuffix

    inner

let translateInstruction vmFileName =
    VirtualMachine.parse vmFileName
    >> map (Translator.translate (createLabelStream "__AUTO_LABEL"))
    >> map Assembly.serialiseList
