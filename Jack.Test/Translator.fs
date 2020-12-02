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
                       [ AI(Value 5)
                         CI([ D ], Just A, NoJump)
                         AI(MemorySegmentPointer StackPointer)
                         CI([ M ], Inc M, NoJump)
                         CI([ A ], Dec M, NoJump)
                         CI([ M ], Just D, NoJump) ] |]

                yield
                    [| VirtualMachine.Push(VirtualMachine.Argument 2)
                       [ AI(Value 2)
                         CI([ D ], Just A, NoJump)
                         AI(MemorySegmentPointer Argument)
                         CI([ A ], Add(M, D), NoJump)
                         CI([ D ], Just M, NoJump)
                         AI(MemorySegmentPointer StackPointer)
                         CI([ M ], Inc M, NoJump)
                         CI([ A ], Dec M, NoJump)
                         CI([ M ], Just D, NoJump) ] |]

                yield
                    [| VirtualMachine.Push(VirtualMachine.Static "Hello.0")
                       [ AI(Variable "Hello.0")
                         CI([ D ], Just M, NoJump)
                         AI(MemorySegmentPointer StackPointer)
                         CI([ M ], Inc M, NoJump)
                         CI([ A ], Dec M, NoJump)
                         CI([ M ], Just D, NoJump) ] |]

                yield
                    [| VirtualMachine.Push(VirtualMachine.Pointer VirtualMachine.ToThis)
                       [ AI(MemorySegmentPointer This)
                         CI([ D ], Just M, NoJump)
                         AI(MemorySegmentPointer StackPointer)
                         CI([ M ], Inc M, NoJump)
                         CI([ A ], Dec M, NoJump)
                         CI([ M ], Just D, NoJump) ] |]
            }

        [<Theory; MemberData("testCases")>]
        let Test (instruction: VirtualMachine.Instruction, expected: Assembly.Instruction list) =
            let result = translate (fun () -> "") instruction
            Assert.StrictEqual(expected, result)


    module Pop =
        let testCases: obj [] seq =
            seq {
                yield
                    [| VirtualMachine.Pop(VirtualMachine.Local 99)
                       [ AI(Value 99)
                         CI([ D ], Just A, NoJump)
                         AI(MemorySegmentPointer Local)
                         CI([ D ], Add(M, D), NoJump)
                         AI(MemorySegmentPointer StackPointer)
                         CI([ M ], Dec M, NoJump)
                         CI([ A ], Inc M, NoJump)
                         CI([ M ], Just D, NoJump)
                         CI([ A ], Dec A, NoJump)
                         CI([ D ], Just M, NoJump)
                         CI([ A ], Inc A, NoJump)
                         CI([ M ], Just D, NoJump) ] |]

                yield
                    [| VirtualMachine.Pop(VirtualMachine.Static "Hello.0")
                       [ AI(MemorySegmentPointer StackPointer)
                         CI([ A; M ], Dec M, NoJump)
                         CI([ D ], Just M, NoJump)
                         AI(Variable "Hello.0")
                         CI([ M ], Just D, NoJump) ] |]

                yield
                    [| VirtualMachine.Pop(VirtualMachine.Pointer VirtualMachine.ToThis)
                       [ AI(MemorySegmentPointer StackPointer)
                         CI([ A; M ], Dec M, NoJump)
                         CI([ D ], Just M, NoJump)
                         AI(MemorySegmentPointer This)
                         CI([ M ], Just D, NoJump) ] |]
            }

        [<Theory; MemberData("testCases")>]
        let Test (instruction: VirtualMachine.Instruction, expected: Assembly.Instruction list) =
            let result = translate (fun () -> "") instruction
            Assert.StrictEqual(expected, result)

    module Add =
        let testCases: obj [] seq =
            seq {
                yield
                    [| VirtualMachine.Add
                       [ AI(MemorySegmentPointer StackPointer)
                         CI([ A; M ], Dec M, NoJump)
                         CI([ D ], Just M, NoJump)
                         CI([ A ], Dec A, NoJump)
                         CI([ M ], Add(M, D), NoJump) ] |]
            }

        [<Theory; MemberData("testCases")>]
        let Test (instruction: VirtualMachine.Instruction, expected: Assembly.Instruction list) =
            let result = translate (fun () -> "") instruction
            Assert.StrictEqual(expected, result)

    module Negate =
        let testCases: obj [] seq =
            seq {
                yield
                    [| VirtualMachine.Negate
                       [ AI(MemorySegmentPointer StackPointer)
                         CI([ A ], Dec M, NoJump)
                         CI([ M ], Negate M, NoJump) ] |]
            }

        [<Theory; MemberData("testCases")>]
        let Test (instruction: VirtualMachine.Instruction, expected: Assembly.Instruction list) =
            let result = translate (fun () -> "") instruction
            Assert.StrictEqual(expected, result)

    module GreaterThan =
        let testCases: obj [] seq =
            seq {
                yield
                    [| VirtualMachine.GreaterThan
                       [ AI(MemorySegmentPointer StackPointer)
                         CI([ A; M ], Dec M, NoJump)
                         CI([ D ], Just M, NoJump)
                         CI([ A ], Dec A, NoJump)
                         CI([ D ], Sub(M, D), NoJump)
                         AI(Variable "label_if_true")
                         CI([], Just D, GreaterStrict)
                         AI(Variable "label_end_if")
                         CI([ D ], Zero, JumpAnyway)
                         Label "label_if_true"
                         CI([ D ], One, NoJump)
                         Label "label_end_if" ] |]
            }

        [<Theory; MemberData("testCases")>]
        let Test (instruction: VirtualMachine.Instruction, expected: Assembly.Instruction list) =
            let result =
                translate (fun () -> "label") instruction

            Assert.StrictEqual(expected, result)
