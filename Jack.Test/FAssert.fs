module Jack.Test.FAssert

open Xunit

let Unwrap result =
    match result with
    | Ok r -> r
    | Error e -> failwith e

let StrictEqual expected result = Assert.StrictEqual(expected, result)
