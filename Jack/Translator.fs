module Jack.Translator

open Jack.Assembly

let TempBaseAddress = 5

let push location =
    let loadOffset offset =
        [ AI(Value offset)
          CI([ D ], Just A, NoJump) ]

    let loadValueWithOffsetFromSegment symbol =
        [ AI(MemorySegmentPointer symbol)
          CI([ A ], Add(M, D), NoJump)
          CI([ D ], Just M, NoJump) ]

    let loadValueFrom addr = [ AI(addr); CI([ D ], Just M, NoJump) ]

    let writeToStack =
        [ AI(MemorySegmentPointer StackPointer)
          CI([ M ], Inc M, NoJump)
          CI([ A ], Dec M, NoJump)
          CI([ M ], Just D, NoJump) ]

    match location with
    | VirtualMachine.Local offset ->
        loadOffset offset
        @ loadValueWithOffsetFromSegment Local
        @ writeToStack
    | VirtualMachine.Argument offset ->
        loadOffset offset
        @ loadValueWithOffsetFromSegment Argument
        @ writeToStack
    | VirtualMachine.This offset ->
        loadOffset offset
        @ loadValueWithOffsetFromSegment This
        @ writeToStack
    | VirtualMachine.That offset ->
        loadOffset offset
        @ loadValueWithOffsetFromSegment That
        @ writeToStack
    | VirtualMachine.Constant c -> loadOffset c @ writeToStack
    | VirtualMachine.Static name -> loadValueFrom (Variable name) @ writeToStack
    | VirtualMachine.Pointer p ->
        match p with
        | VirtualMachine.ToThis ->
            loadValueFrom (MemorySegmentPointer This)
            @ writeToStack
        | VirtualMachine.ToThat ->
            loadValueFrom (MemorySegmentPointer That)
            @ writeToStack
    | VirtualMachine.Temp offset ->
        [ AI(Value(TempBaseAddress + offset))
          CI([ D ], Just M, NoJump) ]
        @ writeToStack

let pop location =
    let popToPointedSegment segment offset =
        [ AI(Value offset)
          CI([ D ], Just A, NoJump)
          AI(MemorySegmentPointer segment)
          CI([ D ], Add(M, D), NoJump)
          AI(MemorySegmentPointer StackPointer)
          CI([ M ], Dec M, NoJump)
          CI([ A ], Inc M, NoJump)
          CI([ M ], Just D, NoJump)
          CI([ A ], Dec A, NoJump)
          CI([ D ], Just M, NoJump)
          CI([ A ], Inc A, NoJump)
          CI([ M ], Just D, NoJump) ]

    let popToFixedSegment addr =
        [ AI(MemorySegmentPointer StackPointer)
          CI([ A; M ], Dec M, NoJump)
          CI([ D ], Just M, NoJump)
          AI(addr)
          CI([ M ], Just D, NoJump) ]

    match location with
    | VirtualMachine.Local offset -> popToPointedSegment Local offset
    | VirtualMachine.Argument offset -> popToPointedSegment Argument offset
    | VirtualMachine.This offset -> popToPointedSegment This offset
    | VirtualMachine.That offset -> popToPointedSegment That offset
    | VirtualMachine.Constant _ -> failwith "pop to constant segment is illegal"
    | VirtualMachine.Static name -> Variable name |> popToFixedSegment
    | VirtualMachine.Pointer p ->
        match p with
        | VirtualMachine.ToThis -> MemorySegmentPointer This |> popToFixedSegment
        | VirtualMachine.ToThat -> MemorySegmentPointer That |> popToFixedSegment
    | VirtualMachine.Temp offset ->
        Value(TempBaseAddress + offset)
        |> popToFixedSegment

let operator1 operator =
    [ AI(MemorySegmentPointer StackPointer)
      CI([ A ], Dec M, NoJump)
      CI([ M ], operator M, NoJump) ]

let operator2 operator =
    [ AI(MemorySegmentPointer StackPointer)
      CI([ A; M ], Dec M, NoJump)
      CI([ D ], Just M, NoJump)
      CI([ A ], Dec A, NoJump)
      CI([ M ], operator (M, D), NoJump) ]

let compare2 jmp freeLabel =
    let ifTrue = freeLabel + "_if_true"
    let endIf = freeLabel + "_end_if"
    [ AI(MemorySegmentPointer StackPointer)
      CI([ A; M ], Dec M, NoJump)
      CI([ D ], Just M, NoJump)
      CI([ A ], Dec A, NoJump)
      CI([ D ], Sub(M, D), NoJump)
      AI(Variable ifTrue)
      CI([], Just D, jmp)
      AI(Variable endIf)
      CI([ D ], Zero, JumpAnyway)
      Label ifTrue
      CI([ D ], One, NoJump)
      Label endIf ]



let translate labelStream instruction =
    match instruction with
    | VirtualMachine.Push location -> push location
    | VirtualMachine.Pop location -> pop location
    | VirtualMachine.Add -> operator2 Assembly.Add
    | VirtualMachine.Subtract -> operator2 Assembly.Sub
    | VirtualMachine.Negate -> operator1 Assembly.Negate
    | VirtualMachine.Equal -> labelStream () |> compare2 Assembly.Equal
    | VirtualMachine.GreaterThan -> labelStream () |> compare2 Assembly.GreaterStrict
    | VirtualMachine.LessThan -> labelStream () |> compare2 Assembly.LessStrict
    | VirtualMachine.And -> operator2 Assembly.And
    | VirtualMachine.Or -> operator2 Assembly.Or
    | VirtualMachine.Not -> operator1 Assembly.Not
