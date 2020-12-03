namespace Jack.Test.VirtualMachine

open Xunit
open Jack
open VirtualMachine

module FAssert =
    let Unwrap result =
        match result with
        | Ok r -> r
        | Error e -> failwith e

    let StrictEqual expected result = Assert.StrictEqual(expected, result)

module parse =

    let testCases: obj [] seq =
        seq {
            yield
                [| "push constant 4"
                   Push(Constant 4) |]
            yield [| "push local 0"; Push(Local 0) |]
            yield
                [| "push static 2"
                   Push(Static "exampleFile.2") |]
            yield [| "pop local 99"; Pop(Local 99) |]
            yield [| "add"; Add |]
            yield [| "neg"; Negate |]
            yield [| "and"; And |]
            yield [| "or"; Or |]
        }

    [<Theory; MemberData("testCases")>]
    let TestOk (instruction: string, expected: VirtualMachine.Instruction) =
        parse "exampleFile" instruction
        |> FAssert.Unwrap
        |> FAssert.StrictEqual expected
