namespace Jack.Test.Compiler

open Xunit
open Jack
open UseCases

module createLabelStream =

    [<Fact>]
    let Test () =
        let ls = createLabelStream "BASE_LABEL"
        Assert.StrictEqual("BASE_LABEL_0", ls ())
        Assert.StrictEqual("BASE_LABEL_1", ls ())
        Assert.StrictEqual("BASE_LABEL_2", ls ())
