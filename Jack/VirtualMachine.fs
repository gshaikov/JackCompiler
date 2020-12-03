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

let private parse1 instruction =
    match instruction with
    | "add" -> Ok(Add)
    | "sub" -> Ok(Subtract)
    | "neg" -> Ok(Negate)
    | "eq" -> Ok(Equal)
    | "gt" -> Ok(GreaterThan)
    | "lt" -> Ok(LessThan)
    | "and" -> Ok(And)
    | "or" -> Ok(Or)
    | "not" -> Ok(Not)
    | _ ->
        sprintf "unknown instruction: %s" instruction
        |> Error

let private parse3 fileName (instruction: string array) =
    let parse3cmd cmd =
        match cmd with
        | "push" -> Push |> Ok
        | "pop" -> Pop |> Ok
        | _ -> sprintf "unknown instruction: %s" cmd |> Error

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
            | _ ->
                sprintf "unknown pointer location: %s" addr
                |> Error
        | "temp" -> addr |> int |> Temp |> Ok
        | _ -> sprintf "unknown segment: %s" segment |> Error

    match parse3cmd instruction.[0], parse3seg fileName instruction.[1] instruction.[2] with
    | Ok cmd, Ok loc -> cmd loc |> Ok
    | Error e, Ok _ -> Error e
    | Ok _, Error e -> Error e
    | Error e1, Error e2 -> Error(e1 + "; " + e2)

let parse fileName (instruction: string) =
    let sarr = instruction.Split [| ' ' |]
    let arrLen = Array.length sarr
    if arrLen = 1 then
        parse1 instruction
    elif arrLen = 3 then
        parse3 fileName sarr
    else
        sprintf "string has wrong number of words: %i" arrLen
        |> Error
