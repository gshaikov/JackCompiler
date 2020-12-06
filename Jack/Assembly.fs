module Jack.Assembly

// Reg is a register module
module Reg =

    type T =
        | A
        | M
        | D
        member this.ToAssembly = this.ToString

module Address =

    type MemorySegmentSymbol =
        | StackPointer
        | Local
        | Argument
        | This
        | That
        member this.ToAssembly() =
            match this with
            | StackPointer -> "SP"
            | Local -> "LCL"
            | Argument -> "ARG"
            | This -> "THIS"
            | That -> "THAT"

    type T =
        | Value of int
        | MemorySegmentPointer of MemorySegmentSymbol
        | Variable of string
        member this.ToAssembly() =
            match this with
            | Value num -> num |> string
            | MemorySegmentPointer seg -> seg.ToAssembly()
            | Variable s -> s

type Dest =
    | M
    | D
    | MD
    | A
    | AM
    | AD
    | AMD
    | NoDest
    member this.ToAssembly() =
        match this with
        | M -> Some("M")
        | D -> Some("D")
        | MD -> Some("MD")
        | A -> Some("A")
        | AM -> Some("AM")
        | AD -> Some("AD")
        | AMD -> Some("AMD")
        | NoDest -> None

type Comp =
    | Zero
    | One
    | MinusOne
    | Just of Reg.T
    | Not of Reg.T
    | Negate of Reg.T
    | Inc of Reg.T
    | Dec of Reg.T
    | Add of Reg.T * Reg.T
    | Sub of Reg.T * Reg.T
    | And of Reg.T * Reg.T
    | Or of Reg.T * Reg.T
    member this.ToAssembly() =
        match this with
        | Zero -> "0"
        | One -> "1"
        | MinusOne -> "-1"
        | Just reg -> reg.ToAssembly()
        | Not reg -> "!" + reg.ToAssembly()
        | Negate reg -> "-" + reg.ToAssembly()
        | Inc reg -> reg.ToAssembly() + "+1"
        | Dec reg -> reg.ToAssembly() + "-1"
        | Add (Reg.D, reg)
        | Add (reg, Reg.D) -> "D+" + reg.ToAssembly()
        | Sub (Reg.D, reg) -> "D-" + reg.ToAssembly()
        | Sub (reg, Reg.D) -> reg.ToAssembly() + "-D"
        | And (Reg.D, reg)
        | And (reg, Reg.D) -> "D&" + reg.ToAssembly()
        | Or (Reg.D, reg)
        | Or (reg, Reg.D) -> "D|" + reg.ToAssembly()
        | _ -> failwithf "illegal operation %A" this

type Jump =
    | GreaterStrict
    | Equal
    | GreaterEqual
    | LessStrict
    | NotEqual
    | LessEqual
    | JumpAnyway
    | NoJump
    member this.ToAssembly() =
        match this with
        | GreaterStrict -> Some("JGT")
        | Equal -> Some("JEQ")
        | GreaterEqual -> Some("JGE")
        | LessStrict -> Some("JLT")
        | NotEqual -> Some("JNE")
        | LessEqual -> Some("JLE")
        | JumpAnyway -> Some("JMP")
        | NoJump -> None

type Instruction =
    | Comment of string
    | AI of Address.T
    | CI of Dest * Comp * Jump
    | Label of string
    member this.ToAssembly() =
        match this with
        | Comment line -> line
        | AI addr -> "@" + addr.ToAssembly()
        | CI (dest, comp, jump) ->
            match (dest.ToAssembly(), jump.ToAssembly()) with
            | Some d, Some j -> d + "=" + comp.ToAssembly() + ";" + j
            | Some d, None -> d + "=" + comp.ToAssembly()
            | None, Some j -> comp.ToAssembly() + ";" + j
            | None, None -> comp.ToAssembly()
        | Label lbl -> "(" + lbl + ")"

let serialise (instruction: Instruction) = instruction.ToAssembly()
