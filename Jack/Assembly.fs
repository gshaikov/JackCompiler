module Jack.Assembly

type MemorySegmentSymbol =
    | StackPointer
    | Local
    | Argument
    | This
    | That

type Address =
    | Value of int
    | MemorySegmentPointer of MemorySegmentSymbol
    | Variable of string

type Register =
    | A
    | M
    | D

type Destination = Register list

type Computation =
    | Zero
    | One
    | MinusOne
    | Just of Register
    | Not of Register
    | Negate of Register
    | Inc of Register
    | Dec of Register
    | Add of Register * Register
    | Sub of Register * Register
    | And of Register * Register
    | Or of Register * Register

type Jump =
    | GreaterStrict
    | Equal
    | GreaterEqual
    | LessStrict
    | NotEqual
    | LessEqual
    | JumpAnyway
    | NoJump

type Instruction =
    | AI of Address
    | CI of Destination * Computation * Jump
    | Label of string
