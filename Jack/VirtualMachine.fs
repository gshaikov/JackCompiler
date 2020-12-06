module Jack.VirtualMachine

type Ptr =
    | ToThis
    | ToThat

type MemorySegment =
    | Local of int
    | Argument of int
    | This of int
    | That of int
    | Constant of int
    | Static of string
    | Pointer of Ptr
    | Temp of int

type Instruction =
    | Push of MemorySegment
    | Pop of MemorySegment
    | Add
    | Subtract
    | Negate
    | Equal
    | GreaterThan
    | LessThan
    | And
    | Or
    | Not

type ParseResult =
    | POk of Instruction
    | PNone
    | PError of string

let private parse1 instruction =
    match instruction with
    | "add" -> POk(Add)
    | "sub" -> POk(Subtract)
    | "neg" -> POk(Negate)
    | "eq" -> POk(Equal)
    | "gt" -> POk(GreaterThan)
    | "lt" -> POk(LessThan)
    | "and" -> POk(And)
    | "or" -> POk(Or)
    | "not" -> POk(Not)
    | _ -> PError(sprintf "unknown instruction: %s" instruction)

let private parse3 fileName (instruction: string array) =
    let parse3cmd cmd =
        match cmd with
        | "push" -> Push |> Ok
        | "pop" -> Pop |> Ok
        | _ -> Error(sprintf "unknown instruction: %s" cmd)

    let parse3seg fileName segment addr =
        match segment with
        | "local" -> addr |> int |> Local |> Ok
        | "argument" -> addr |> int |> Argument |> Ok
        | "this" -> addr |> int |> This |> Ok
        | "that" -> addr |> int |> That |> Ok
        | "constant" -> addr |> int |> Constant |> Ok
        | "static" -> fileName + "." + addr |> Static |> Ok
        | "pointer" ->
            match addr with
            | "0" -> Pointer ToThis |> Ok
            | "1" -> Pointer ToThat |> Ok
            | _ -> Error(sprintf "unknown pointer location: %s" addr)
        | "temp" -> addr |> int |> Temp |> Ok
        | _ -> Error(sprintf "unknown segment: %s" segment)

    match parse3cmd instruction.[0], parse3seg fileName instruction.[1] instruction.[2] with
    | Ok cmd, Ok loc -> cmd loc |> POk
    | Error e, Ok _ -> PError e
    | Ok _, Error e -> PError e
    | Error e1, Error e2 -> PError(e1 + "; " + e2)

module StringUtils =

    let rec tryTake count (source: string) =
        match Seq.tryItem (count - 1) source with
        | Some _ ->
            source
            |> Seq.take count
            |> System.String.Concat
            |> Some
        | None -> None

    let startsWith pattern line =
        match tryTake (String.length pattern) line with
        | Some p when p = pattern -> true
        | _ -> false

let parse fileName instruction =
    if (String.length instruction = 0
        || StringUtils.startsWith "//" instruction
        || StringUtils.startsWith "\n" instruction) then
        PNone
    else
        let sarr = instruction.Split [| ' ' |]
        let arrLen = Array.length sarr
        if arrLen = 1
        then parse1 instruction
        elif arrLen = 3
        then parse3 fileName sarr
        else PError(sprintf "instruction has wrong number of tokens (%i): \"%s\"" arrLen instruction)
