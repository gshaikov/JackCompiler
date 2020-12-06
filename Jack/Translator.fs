module Jack.Translator

open Jack.Assembly

let TempBaseAddress = 5

let push location =
    let loadOffset offset =
        [ AI(Address.Value offset)
          CI(D, Just Reg.A, NoJump) ]

    let loadValueWithOffsetFromSegment symbol =
        [ AI(Address.MemorySegmentPointer symbol)
          CI(A, Add(Reg.M, Reg.D), NoJump)
          CI(D, Just Reg.M, NoJump) ]

    let loadValueFrom addr = [ AI(addr); CI(D, Just Reg.M, NoJump) ]

    let writeToStack =
        [ AI(Address.MemorySegmentPointer Address.StackPointer)
          CI(M, Inc Reg.M, NoJump)
          CI(A, Dec Reg.M, NoJump)
          CI(M, Just Reg.D, NoJump) ]

    match location with
    | VirtualMachine.Local offset ->
        loadOffset offset
        @ loadValueWithOffsetFromSegment Address.Local
        @ writeToStack
    | VirtualMachine.Argument offset ->
        loadOffset offset
        @ loadValueWithOffsetFromSegment Address.Argument
        @ writeToStack
    | VirtualMachine.This offset ->
        loadOffset offset
        @ loadValueWithOffsetFromSegment Address.This
        @ writeToStack
    | VirtualMachine.That offset ->
        loadOffset offset
        @ loadValueWithOffsetFromSegment Address.That
        @ writeToStack
    | VirtualMachine.Constant c -> loadOffset c @ writeToStack
    | VirtualMachine.Static name ->
        loadValueFrom (Address.Variable name)
        @ writeToStack
    | VirtualMachine.Pointer p ->
        match p with
        | VirtualMachine.ToThis ->
            loadValueFrom (Address.MemorySegmentPointer Address.This)
            @ writeToStack
        | VirtualMachine.ToThat ->
            loadValueFrom (Address.MemorySegmentPointer Address.That)
            @ writeToStack
    | VirtualMachine.Temp offset ->
        [ AI(Address.Value(TempBaseAddress + offset))
          CI(D, Just Reg.M, NoJump) ]
        @ writeToStack

let pop location =
    let popToPointedSegment segment offset =
        [ AI(Address.Value offset)
          CI(D, Just Reg.A, NoJump)
          AI(Address.MemorySegmentPointer segment)
          CI(D, Add(Reg.M, Reg.D), NoJump)
          AI(Address.MemorySegmentPointer Address.StackPointer)
          CI(M, Dec Reg.M, NoJump)
          CI(A, Inc Reg.M, NoJump)
          CI(M, Just Reg.D, NoJump)
          CI(A, Dec Reg.A, NoJump)
          CI(D, Just Reg.M, NoJump)
          CI(A, Inc Reg.A, NoJump)
          CI(M, Just Reg.D, NoJump) ]

    let popToFixedSegment addr =
        [ AI(Address.MemorySegmentPointer Address.StackPointer)
          CI(AM, Dec Reg.M, NoJump)
          CI(D, Just Reg.M, NoJump)
          AI(addr)
          CI(M, Just Reg.D, NoJump) ]

    match location with
    | VirtualMachine.Local offset -> popToPointedSegment Address.Local offset
    | VirtualMachine.Argument offset -> popToPointedSegment Address.Argument offset
    | VirtualMachine.This offset -> popToPointedSegment Address.This offset
    | VirtualMachine.That offset -> popToPointedSegment Address.That offset
    | VirtualMachine.Constant _ -> failwith "pop to constant segment is illegal"
    | VirtualMachine.Static name -> Address.Variable name |> popToFixedSegment
    | VirtualMachine.Pointer p ->
        match p with
        | VirtualMachine.ToThis ->
            Address.MemorySegmentPointer Address.This
            |> popToFixedSegment
        | VirtualMachine.ToThat ->
            Address.MemorySegmentPointer Address.That
            |> popToFixedSegment
    | VirtualMachine.Temp offset ->
        Address.Value(TempBaseAddress + offset)
        |> popToFixedSegment

let operator1 operator =
    [ AI(Address.MemorySegmentPointer Address.StackPointer)
      CI(A, Dec Reg.M, NoJump)
      CI(M, operator Reg.M, NoJump) ]

let operator2 operator =
    [ AI(Address.MemorySegmentPointer Address.StackPointer)
      CI(AM, Dec Reg.M, NoJump)
      CI(D, Just Reg.M, NoJump)
      CI(A, Dec Reg.A, NoJump)
      CI(M, operator (Reg.M, Reg.D), NoJump) ]

let compare2 jmp freeLabel =
    let ifTrue = freeLabel + "_if_true"
    let endIf = freeLabel + "_end_if"
    [ AI(Address.MemorySegmentPointer Address.StackPointer)
      CI(AM, Dec Reg.M, NoJump)
      CI(D, Just Reg.M, NoJump)
      CI(A, Dec Reg.A, NoJump)
      CI(D, Sub(Reg.M, Reg.D), NoJump)
      AI(Address.Variable ifTrue)
      CI(NoDest, Just Reg.D, jmp)
      AI(Address.Variable endIf)
      CI(D, Zero, JumpAnyway)
      Label ifTrue
      CI(D, MinusOne, NoJump)
      Label endIf
      AI(Address.MemorySegmentPointer Address.StackPointer)
      CI(A, Dec Reg.M, NoJump)
      CI(M, Just Reg.D, NoJump) ]



let translate freeLabel instruction =
    match instruction with
    | VirtualMachine.Push location -> push location
    | VirtualMachine.Pop location -> pop location
    | VirtualMachine.Add -> operator2 Assembly.Add
    | VirtualMachine.Subtract -> operator2 Assembly.Sub
    | VirtualMachine.Negate -> operator1 Assembly.Negate
    | VirtualMachine.Equal -> freeLabel |> compare2 Assembly.Equal
    | VirtualMachine.GreaterThan -> freeLabel |> compare2 Assembly.GreaterStrict
    | VirtualMachine.LessThan -> freeLabel |> compare2 Assembly.LessStrict
    | VirtualMachine.And -> operator2 Assembly.And
    | VirtualMachine.Or -> operator2 Assembly.Or
    | VirtualMachine.Not -> operator1 Assembly.Not
