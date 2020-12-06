namespace Jack.Test.Translator

open Xunit
open Jack
open Translator

module translate =

    open Assembly

    module Push =
        let testCases: obj [] seq =
            seq {
                yield
                    [| VirtualMachine.Push(VirtualMachine.Constant 5)
                       [ AI(Address.Value 5)
                         CI(D, Just Reg.A, NoJump)
                         AI(Address.MemorySegmentPointer Address.StackPointer)
                         CI(M, Inc Reg.M, NoJump)
                         CI(A, Dec Reg.M, NoJump)
                         CI(M, Just Reg.D, NoJump) ] |]

                yield
                    [| VirtualMachine.Push(VirtualMachine.Argument 2)
                       [ AI(Address.Value 2)
                         CI(D, Just Reg.A, NoJump)
                         AI(Address.MemorySegmentPointer Address.Argument)
                         CI(A, Add(Reg.M, Reg.D), NoJump)
                         CI(D, Just Reg.M, NoJump)
                         AI(Address.MemorySegmentPointer Address.StackPointer)
                         CI(M, Inc Reg.M, NoJump)
                         CI(A, Dec Reg.M, NoJump)
                         CI(M, Just Reg.D, NoJump) ] |]

                yield
                    [| VirtualMachine.Push(VirtualMachine.Static "Hello.0")
                       [ AI(Address.Variable "Hello.0")
                         CI(D, Just Reg.M, NoJump)
                         AI(Address.MemorySegmentPointer Address.StackPointer)
                         CI(M, Inc Reg.M, NoJump)
                         CI(A, Dec Reg.M, NoJump)
                         CI(M, Just Reg.D, NoJump) ] |]

                yield
                    [| VirtualMachine.Push(VirtualMachine.Pointer VirtualMachine.ToThis)
                       [ AI(Address.MemorySegmentPointer Address.This)
                         CI(D, Just Reg.M, NoJump)
                         AI(Address.MemorySegmentPointer Address.StackPointer)
                         CI(M, Inc Reg.M, NoJump)
                         CI(A, Dec Reg.M, NoJump)
                         CI(M, Just Reg.D, NoJump) ] |]
            }

        [<Theory; MemberData("testCases")>]
        let Test (instruction: VirtualMachine.Instruction, expected: Assembly.Instruction list) =
            let result = translate "" instruction
            Assert.StrictEqual(expected, result)


    module Pop =
        let testCases: obj [] seq =
            seq {
                yield
                    [| VirtualMachine.Pop(VirtualMachine.Local 99)
                       [ AI(Address.Value 99)
                         CI(D, Just Reg.A, NoJump)
                         AI(Address.MemorySegmentPointer Address.Local)
                         CI(D, Add(Reg.M, Reg.D), NoJump)
                         AI(Address.MemorySegmentPointer Address.StackPointer)
                         CI(M, Dec Reg.M, NoJump)
                         CI(A, Inc Reg.M, NoJump)
                         CI(M, Just Reg.D, NoJump)
                         CI(A, Dec Reg.A, NoJump)
                         CI(D, Just Reg.M, NoJump)
                         CI(A, Inc Reg.A, NoJump)
                         CI(M, Just Reg.D, NoJump) ] |]

                yield
                    [| VirtualMachine.Pop(VirtualMachine.Static "Hello.0")
                       [ AI(Address.MemorySegmentPointer Address.StackPointer)
                         CI(AM, Dec Reg.M, NoJump)
                         CI(D, Just Reg.M, NoJump)
                         AI(Address.Variable "Hello.0")
                         CI(M, Just Reg.D, NoJump) ] |]

                yield
                    [| VirtualMachine.Pop(VirtualMachine.Pointer VirtualMachine.ToThis)
                       [ AI(Address.MemorySegmentPointer Address.StackPointer)
                         CI(AM, Dec Reg.M, NoJump)
                         CI(D, Just Reg.M, NoJump)
                         AI(Address.MemorySegmentPointer Address.This)
                         CI(M, Just Reg.D, NoJump) ] |]
            }

        [<Theory; MemberData("testCases")>]
        let Test (instruction: VirtualMachine.Instruction, expected: Assembly.Instruction list) =
            let result = translate "" instruction
            Assert.StrictEqual(expected, result)

    module Add =
        let testCases: obj [] seq =
            seq {
                yield
                    [| VirtualMachine.Add
                       [ AI(Address.MemorySegmentPointer Address.StackPointer)
                         CI(AM, Dec Reg.M, NoJump)
                         CI(D, Just Reg.M, NoJump)
                         CI(A, Dec Reg.A, NoJump)
                         CI(M, Add(Reg.M, Reg.D), NoJump) ] |]
            }

        [<Theory; MemberData("testCases")>]
        let Test (instruction: VirtualMachine.Instruction, expected: Assembly.Instruction list) =
            let result = translate "" instruction
            Assert.StrictEqual(expected, result)

    module Negate =
        let testCases: obj [] seq =
            seq {
                yield
                    [| VirtualMachine.Negate
                       [ AI(Address.MemorySegmentPointer Address.StackPointer)
                         CI(A, Dec Reg.M, NoJump)
                         CI(M, Negate Reg.M, NoJump) ] |]
            }

        [<Theory; MemberData("testCases")>]
        let Test (instruction: VirtualMachine.Instruction, expected: Assembly.Instruction list) =
            let result = translate "" instruction
            Assert.StrictEqual(expected, result)

    module GreaterThan =
        let testCases: obj [] seq =
            seq {
                yield
                    [| VirtualMachine.GreaterThan
                       [ AI(Address.MemorySegmentPointer Address.StackPointer)
                         CI(AM, Dec Reg.M, NoJump)
                         CI(D, Just Reg.M, NoJump)
                         CI(A, Dec Reg.A, NoJump)
                         CI(D, Sub(Reg.M, Reg.D), NoJump)
                         AI(Address.Variable "label_if_true")
                         CI(NoDest, Just Reg.D, GreaterStrict)
                         AI(Address.Variable "label_end_if")
                         CI(D, Zero, JumpAnyway)
                         Label "label_if_true"
                         CI(D, MinusOne, NoJump)
                         Label "label_end_if"
                         AI(Address.MemorySegmentPointer Address.StackPointer)
                         CI(A, Dec Reg.M, NoJump)
                         CI(M, Just Reg.D, NoJump) ] |]
            }

        [<Theory; MemberData("testCases")>]
        let Test (instruction: VirtualMachine.Instruction, expected: Assembly.Instruction list) =
            let result = translate "label" instruction
            Assert.StrictEqual(expected, result)
