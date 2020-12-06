namespace Jack.Test.Compiler

open Xunit
open Jack.Test

open Jack
open UseCases

module createLabelStream =

    [<Fact>]
    let Test () =
        let ls = createLabelStream "BASE_LABEL"
        Assert.StrictEqual("BASE_LABEL_1", ls ())
        Assert.StrictEqual("BASE_LABEL_2", ls ())
        Assert.StrictEqual("BASE_LABEL_3", ls ())

module translateInstruction =

    let testCases: obj [] seq =
        seq {
            yield
                [| "push local 5"
                   "@5\nD=A\n@LCL\nA=D+M\nD=M\n@SP\nM=M+1\nA=M-1\nM=D\n" |]

            yield
                [| "gt"
                   "@SP\nAM=M-1\nD=M\nA=A-1\nD=M-D\n@__AUTO_LABEL_1_if_true\nD;JGT\n@__AUTO_LABEL_1_end_if\nD=0;JMP\n(__AUTO_LABEL_1_if_true)\nD=1\n(__AUTO_LABEL_1_end_if)\n" |]
        }

    [<Theory; MemberData("testCases")>]
    let Test (instruction: string, expected: string) =
        instruction
        |> translateInstruction "Foo"
        |> FAssert.Unwrap
        |> FAssert.StrictEqual expected
