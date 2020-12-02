module Jack.UseCases

let createLabelStream baseLabel =
    let mutable s =
        Seq.initInfinite (fun i -> sprintf "%s_%i" baseLabel i)

    let inner () =
        let res = Seq.head s
        s <- Seq.tail s
        res

    inner
