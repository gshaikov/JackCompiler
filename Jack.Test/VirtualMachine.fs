namespace Jack.Test.VirtualMachine

open Xunit
open Jack.Test

open Jack
open VirtualMachine

module StringUtils =

    [<Fact>]
    let TestTryTake () =
        StringUtils.tryTake 2 "abcd"
        |> FAssert.StrictEqual(Some("ab"))
        StringUtils.tryTake 2 "d"
        |> FAssert.StrictEqual None
        StringUtils.tryTake 2 ""
        |> FAssert.StrictEqual None

    [<Fact>]
    let TestStartsWith () =
        StringUtils.startsWith "abc" "abcd"
        |> FAssert.StrictEqual true
        StringUtils.startsWith "abc" "dsfsd"
        |> FAssert.StrictEqual false
        StringUtils.startsWith "abc" ""
        |> FAssert.StrictEqual false

module parse =

    let testCases: obj [] seq =
        seq {
            yield [| ""; PNone |]
            yield [| "// something"; PNone |]
            yield
                [| "push constant 4"
                   POk(Push(Constant 4)) |]
            yield [| "push local 0"; POk(Push(Local 0)) |]
            yield
                [| "push static 2"
                   POk(Push(Static "exampleFile.2")) |]
            yield [| "pop local 99"; POk(Pop(Local 99)) |]
            yield [| "add"; POk(Add) |]
            yield [| "neg"; POk(Negate) |]
            yield [| "and"; POk(And) |]
            yield [| "or"; POk(Or) |]
        }

    [<Theory; MemberData("testCases")>]
    let Test (instruction, expected) =
        parse "exampleFile" instruction
        |> FAssert.StrictEqual expected
