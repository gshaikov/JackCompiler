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
