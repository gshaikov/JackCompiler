namespace Jack.Test.Assembly

open Xunit
open Jack.Test

open Jack
open Assembly

module ToAssembly =

    let testCases: obj [] seq =
        seq {
            yield [| AI(Address.Value 5); "@5" |]
            yield
                [| AI(Address.MemorySegmentPointer Address.Local)
                   "@LCL" |]
            yield
                [| AI(Address.Variable "Foo.5")
                   "@Foo.5" |]
            yield
                [| CI(NoDest, Just Reg.D, NoJump)
                   "D" |]
            yield
                [| CI(NoDest, Add(Reg.D, Reg.M), NoJump)
                   "D+M" |]
            yield [| CI(D, Just Reg.D, NoJump); "D=D" |]
            yield [| CI(M, Just Reg.D, NoJump); "M=D" |]
            yield [| CI(D, Just Reg.M, NoJump); "D=M" |]
            yield
                [| CI(M, Add(Reg.D, Reg.M), NoJump)
                   "M=D+M" |]
            yield
                [| CI(NoDest, Just Reg.M, GreaterStrict)
                   "M;JGT" |]
            yield
                [| CI(NoDest, Inc Reg.D, Equal)
                   "D+1;JEQ" |]
            yield
                [| CI(M, Inc Reg.M, NotEqual)
                   "M=M+1;JNE" |]
        }

    [<Theory; MemberData("testCases")>]
    let Test (instruction: Instruction, expected: string) =
        serialise instruction
        |> FAssert.StrictEqual expected
