module LC3VirtualMachine =
    open System
    open System.IO

    exception LC3VirtualMachineException of string

    let inline unexpectedValueException (arg: string) = 
        LC3VirtualMachineException("Unexpected value '" + arg + "'")

    let inline badOpcodeException (arg: string) = 
        LC3VirtualMachineException("Bad opcode '" + arg + "'")

    let inline unknownOpcodeException (arg: string) =
        LC3VirtualMachineException("Unknown opcode '" + arg + "'")

    let inline unknownTrapcodeException (arg: string) =
        LC3VirtualMachineException("Unknown trapcode '" + arg + "'")

    type RegisterTypes = 
        | R_R0    = 0
        | R_R1    = 1
        | R_R2    = 2
        | R_R3    = 3
        | R_R4    = 4
        | R_R5    = 5
        | R_R6    = 6
        | R_R7    = 7
        | R_PC    = 8
        | R_COND  = 9
        | R_COUNT = 10

    type OpcodeTypes = 
        | OP_BR   = 0
        | OP_ADD  = 1
        | OP_LD   = 2
        | OP_ST   = 3
        | OP_JSR  = 4
        | OP_AND  = 5
        | OP_LDR  = 6
        | OP_STR  = 7
        | OP_RTI  = 8
        | OP_NOT  = 9
        | OP_LDI  = 10
        | OP_STI  = 11
        | OP_JMP  = 12
        | OP_RES  = 13
        | OP_LEA  = 14
        | OP_TRAP = 15

    type ConditionFlagTypes = 
        | FL_POS = 1
        | FL_ZRO = 2
        | FL_NEG = 4

    type MemoryMappedRegisterTypes =
        | MR_KBSR = 0xFE00
        | MR_KBDR = 0xFE02

    type TrapcodeTypes = 
        | TRAP_GETC  = 0x20
        | TRAP_OUT   = 0x21
        | TRAP_PUTS  = 0x22
        | TRAP_IN    = 0x23
        | TRAP_PUTSP = 0x24
        | TRAP_HALT  = 0x25

    //
    let unpackOp (instruction: uint16) = (instruction >>> 12)
    
    let unpackDr (instruction: uint16) = (instruction >>> 9) &&& 0x7us
    
    let unpackSr1 (instruction: uint16) = (instruction >>> 6) &&& 0x7us

    let unpackSr2 (instruction: uint16) = (instruction &&& 0x7us)
    
    let unpackImm (instruction: uint16) = (instruction >>> 5) &&& 0x1us

    let unpackLong (instruction: uint16) = (instruction >>> 11) &&& 1us

    let unpackTrap (instruction: uint16) = (instruction &&& 0xFFus)

    let signExtend (value: uint16) (bitCount: int) = 
        if ((value >>> (bitCount - 1)) &&& 1us) > 0us then
            value ||| (0xFFFFus <<< bitCount)
        else
            value

    let swapUInt16 (value: uint16) =
        (value <<< 8) ||| (value >>> 8)

    let readUInt16 (reader: BinaryReader) = 
        if BitConverter.IsLittleEndian then 
            reader.ReadUInt16() |> swapUInt16
        else 
            reader.ReadUInt16()
    
    // Used in debugging only!
    let rec printArray (array: array<uint16>) (lowerBound: int) (upperBound: int) =
            match lowerBound with
            | _ when (lowerBound + 3 < upperBound) -> 
                printf "%5d "   array.[lowerBound]
                printf "%5d "   array.[lowerBound + 1]
                printf "%5d "   array.[lowerBound + 2]
                printf "%5d \n" array.[lowerBound + 3]
                printArray array (lowerBound + 4) upperBound
            | _ when (lowerBound + 2 < upperBound) -> 
                printf "%5d "   array.[lowerBound]
                printf "%5d "   array.[lowerBound + 1]
                printf "%5d \n" array.[lowerBound + 2]
                printArray array (lowerBound + 3) upperBound
            | _ when (lowerBound + 1 < upperBound) -> 
                printf "%5d "   array.[lowerBound]
                printf "%5d \n" array.[lowerBound + 1]
                printArray array (lowerBound + 2) upperBound
            | _ when (lowerBound < upperBound) -> 
                printf "%A \n" array.[lowerBound]
                printArray array (lowerBound + 1) upperBound
            | _ -> ()

    //
    type Memory = array<uint16>

    let memread (memory: Memory) (addr: uint16) = 
        memory.[int addr]

    let memwrite (memory: Memory) (addr: uint16) (value: uint16) = 
        memory.[int addr] <- value
    
    //
    type Registers = array<uint16>

    let regread (registers: Registers) (addr: uint16) = 
        registers.[int addr]

    let regwrite (registers: Registers) (addr: uint16) (value: uint16) = 
        registers.[int addr] <- value

    let regreadpcincr (registers: Registers) =
        let pc = regread registers (uint16 RegisterTypes.R_PC)
        regwrite registers (uint16 RegisterTypes.R_PC) (pc + 1us)
        pc

    let regreadpcdecr (registers: Registers) =
        let pc = regread registers (uint16 RegisterTypes.R_PC)
        regwrite registers (uint16 RegisterTypes.R_PC) (pc - 1us)
        pc

    //
    type VirtualMachine () = 
        let memory: Memory = Array.zeroCreate (int UInt16.MaxValue)
        let registers: Registers = Array.zeroCreate (int RegisterTypes.R_COUNT)

        static let instance = VirtualMachine()

        static member public Instance with get() = instance

        member public this.Memory with get() = memory
        member public this.Registers with get() = registers

        member public this.ReadRegister(addr: uint16) = (regread this.Registers addr)
        member public this.WriteRegister(addr: uint16, value: uint16) = (regwrite this.Registers addr value)

        member public this.ReadMemory(addr: uint16) = (memread this.Memory addr)
        member public this.WriteMemory(addr: uint16, value: uint16) = (memwrite this.Memory addr value)

        member public this.ProgramCounter 
            with get() = this.ReadRegister(uint16 RegisterTypes.R_PC) and set(value: uint16) = this.WriteRegister(uint16 RegisterTypes.R_PC, value)

        member public this.ProgramCounterWithIncrement with get() = (regreadpcincr this.Registers)
        member public this.ProgramCounterWithDecrement with get() = (regreadpcdecr this.Registers)

        member public this.ConditionFlag 
            with get() = this.ReadRegister(uint16 RegisterTypes.R_COND) and set(value: uint16) = this.WriteRegister(uint16 RegisterTypes.R_COND, value)

        member public this.UpdateConditionFlags(r: uint16) = 
            match this.ReadRegister(r) with
            | value when (value = 0us)          -> (this.ConditionFlag <- uint16 ConditionFlagTypes.FL_ZRO)
            | value when ((value >>> 15) > 0us) -> (this.ConditionFlag <- uint16 ConditionFlagTypes.FL_NEG)
            | _                                 -> (this.ConditionFlag <- uint16 ConditionFlagTypes.FL_POS)

        member public this.Load(filename: string) =
            use reader = new BinaryReader(File.Open(filename, FileMode.Open, FileAccess.Read, FileShare.Read))
            
            let origin = readUInt16 reader

            let mutable memoryPtr: uint16 = origin

            while ((reader.BaseStream.Position <> reader.BaseStream.Length) && (memoryPtr < UInt16.MaxValue)) do
                this.Memory.[int memoryPtr] <- readUInt16 reader
                memoryPtr <- memoryPtr + 1us
    
    //
    let rec eval (vm: VirtualMachine) =
        let instruction = vm.ReadMemory vm.ProgramCounterWithIncrement

        match (enum<OpcodeTypes> (int (unpackOp instruction))) with 
        | OpcodeTypes.OP_BR   -> evalOpBr   vm instruction
        | OpcodeTypes.OP_ADD  -> evalOpAdd  vm instruction
        | OpcodeTypes.OP_LD   -> evalOpLd   vm instruction
        | OpcodeTypes.OP_ST   -> evalOpSt   vm instruction
        | OpcodeTypes.OP_JSR  -> evalOpJsr  vm instruction
        | OpcodeTypes.OP_AND  -> evalOpAnd  vm instruction
        | OpcodeTypes.OP_LDR  -> evalOpLdr  vm instruction
        | OpcodeTypes.OP_STR  -> evalOpStr  vm instruction
        | OpcodeTypes.OP_RTI  -> evalOpRti  vm instruction
        | OpcodeTypes.OP_NOT  -> evalOpNot  vm instruction
        | OpcodeTypes.OP_LDI  -> evalOpLdi  vm instruction
        | OpcodeTypes.OP_STI  -> evalOpSti  vm instruction
        | OpcodeTypes.OP_JMP  -> evalOpJmp  vm instruction
        | OpcodeTypes.OP_RES  -> evalOpRes  vm instruction
        | OpcodeTypes.OP_LEA  -> evalOpLea  vm instruction
        | OpcodeTypes.OP_TRAP -> evalOpTrap vm instruction
        | unknownCode ->
            raise (unknownOpcodeException (unknownCode.ToString()))
    and evalOpBr (vm: VirtualMachine) (instruction: uint16) =
        let condFlag = unpackDr instruction
        let pcOffset = signExtend (instruction &&& 0x1FFus) 9
        if (condFlag &&& vm.ConditionFlag) > 0us then
            vm.ProgramCounter <- (vm.ProgramCounter + pcOffset)
        eval vm
    and evalOpAdd (vm: VirtualMachine) (instruction: uint16) =
        let dr = unpackDr instruction
        let sr1 = unpackSr1 instruction
        let imm = unpackImm instruction

        match imm with 
        | 0us -> 
            let sr2 = unpackSr2 instruction
            vm.WriteRegister(dr, (vm.ReadRegister(sr1) + vm.ReadRegister(sr2)))
        | 1us ->
            let immValue = signExtend (instruction &&& 0x1Fus) 5
            vm.WriteRegister(dr, (vm.ReadRegister(sr1) + immValue))
        | _ -> 
            raise (unexpectedValueException (imm.ToString()))
        vm.UpdateConditionFlags(dr)
        eval vm
    and evalOpLd (vm: VirtualMachine) (instruction: uint16) =
        let dr = unpackDr instruction
        let pcOffset = signExtend (instruction &&& 0x1FFus) 9
        vm.WriteRegister(dr, vm.ReadMemory(vm.ProgramCounter + pcOffset))
        vm.UpdateConditionFlags(dr)
        eval vm
    and evalOpSt (vm: VirtualMachine) (instruction: uint16) =
        let sr = unpackDr instruction
        let pcOffset = signExtend (instruction &&& 0x1FFus) 9
        vm.WriteMemory(vm.ProgramCounter + pcOffset, vm.ReadRegister(sr))
        eval vm
    and evalOpJsr (vm: VirtualMachine) (instruction: uint16) =
        let sr1 = unpackSr1 instruction
        let longPcOffset = signExtend (instruction &&& 0x7FFus) 11
        
        vm.WriteRegister(uint16 RegisterTypes.R_R7, vm.ProgramCounter)

        let long = unpackLong instruction
        match long with
        | 0us -> vm.ProgramCounter <- vm.ReadRegister(sr1)
        | 1us -> vm.ProgramCounter <- vm.ProgramCounter + longPcOffset
        | _ -> 
            raise (unexpectedValueException (long.ToString()))
        eval vm
    and evalOpAnd (vm: VirtualMachine) (instruction: uint16) =
        let dr = unpackDr instruction
        let sr1 = unpackSr1 instruction
        let imm = unpackImm instruction
        match imm with 
        | 0us -> 
            let sr2 = unpackSr2 instruction
            vm.WriteRegister(dr, (vm.ReadRegister(sr1) &&& vm.ReadRegister(sr2)))
        | 1us ->
            let immValue = signExtend (instruction &&& 0x1Fus) 5
            vm.WriteRegister(dr, (vm.ReadRegister(sr1) &&& immValue))
        | _ -> 
            raise (unexpectedValueException (imm.ToString()))
        vm.UpdateConditionFlags(dr)
        eval vm
    and evalOpLdr (vm: VirtualMachine) (instruction: uint16) =
        let dr = unpackDr instruction
        let sr1 = unpackSr1 instruction
        let pcOffset = signExtend (instruction &&& 0x3Fus) 6
        vm.WriteRegister(dr, vm.ReadMemory(sr1 + pcOffset))
        vm.UpdateConditionFlags(dr)
        eval vm
    and evalOpStr (vm: VirtualMachine) (instruction: uint16) =
        let dr = unpackDr instruction
        let sr1 = unpackSr1 instruction
        let offset = signExtend (instruction &&& 0x3Fus) 6
        vm.WriteMemory(vm.ReadRegister(sr1) + offset, vm.ReadRegister(dr))
        eval vm
    and evalOpRti (_: VirtualMachine) (_: uint16) =
        raise (badOpcodeException (OpcodeTypes.OP_RTI.ToString()))
    and evalOpNot (vm: VirtualMachine) (instruction: uint16) =
        let dr = unpackDr instruction
        let sr1 = unpackSr1 instruction
        vm.WriteRegister(dr, ~~~(vm.ReadRegister(sr1)))
        vm.UpdateConditionFlags(dr)
        eval vm
    and evalOpLdi (vm: VirtualMachine) (instruction: uint16) =
        let dr = unpackDr instruction
        let pcOffset = signExtend (instruction &&& 0x1FFus) 9
        vm.WriteRegister(dr, 
            vm.ReadMemory(
                vm.ReadMemory(vm.ProgramCounter + pcOffset)))
        vm.UpdateConditionFlags(dr)
        eval vm
    and evalOpSti (vm: VirtualMachine) (instruction: uint16) =
        let dr = unpackDr instruction
        let pcOffset = signExtend (instruction &&& 0x1FFus) 9
        vm.WriteMemory(
            vm.ReadMemory(vm.ProgramCounter + pcOffset), 
                vm.ReadRegister(dr))
        eval vm
    and evalOpJmp (vm: VirtualMachine) (instruction: uint16) =
        let sr1 = unpackSr1 instruction
        vm.ProgramCounter <- vm.ReadRegister(sr1)
        eval vm
    and evalOpRes (_: VirtualMachine) (_: uint16) =
        raise (badOpcodeException (OpcodeTypes.OP_RES.ToString()))
    and evalOpLea (vm: VirtualMachine) (instruction: uint16) =
        let dr = unpackDr instruction
        let pcOffset = signExtend (instruction &&& 0x1FFus) 9
        vm.WriteRegister(dr, vm.ProgramCounter + pcOffset)
        vm.UpdateConditionFlags(dr)
        eval vm
    and evalOpTrap (vm: VirtualMachine) (instruction: uint16) =
        match (enum<TrapcodeTypes> (int (unpackTrap instruction))) with 
        | TrapcodeTypes.TRAP_GETC  -> evalOpTrapGetc  vm instruction
        | TrapcodeTypes.TRAP_OUT   -> evalOpTrapOut   vm instruction
        | TrapcodeTypes.TRAP_PUTS  -> evalOpTrapPuts  vm instruction
        | TrapcodeTypes.TRAP_IN    -> evalOpTrapIn    vm instruction
        | TrapcodeTypes.TRAP_PUTSP -> evalOpTrapPutsp vm instruction
        | TrapcodeTypes.TRAP_HALT  -> evalOpTrapHalt  vm instruction
        | unknownCode -> 
            raise (unknownTrapcodeException (unknownCode.ToString()))
    and evalOpTrapGetc (vm: VirtualMachine) (_: uint16) = 
        vm.WriteRegister(uint16 RegisterTypes.R_R0, uint16 (Console.ReadKey().KeyChar))
        eval vm
    and evalOpTrapOut (vm: VirtualMachine) (_: uint16) = 
        Console.Write(char (vm.ReadRegister(uint16 RegisterTypes.R_R0)))
        eval vm
    and evalOpTrapPuts (vm: VirtualMachine) (_: uint16) = 
        let mutable p = vm.ReadRegister(uint16 RegisterTypes.R_R0)
        let mutable c = vm.ReadMemory(p)
        while c <> 0us do
            Console.Write(char c)
            p <- p + 1us
            c <- vm.ReadMemory(p)
        eval vm
    and evalOpTrapIn (vm: VirtualMachine) (_: uint16) = 
        let c = Console.ReadKey()
        Console.Write(c.KeyChar)
        vm.WriteRegister(uint16 RegisterTypes.R_R0, uint16 c.KeyChar)
        eval vm
    and evalOpTrapPutsp (vm: VirtualMachine) (_: uint16) = 
        let mutable p = vm.ReadRegister(uint16 RegisterTypes.R_R0)
        let mutable c = vm.ReadMemory(p)
        while c <> 0us do
            let char1 = c &&& 0xFFus;
            let char2 = c >>> 8;
            Console.Write(char char1)
            Console.Write(char char2)
            p <- p + 1us
            c <- vm.ReadMemory(p)
        eval vm
    and evalOpTrapHalt (_: VirtualMachine) (_: uint16) = 
        ()
    [<EntryPoint>]
    let main argv =
        VirtualMachine.Instance.ProgramCounter <- 0x3000us
        VirtualMachine.Instance.Load("C:\\Users\\Yuris Liepins\\Projects\\lc3-vm\\images\\2048.obj")

        eval VirtualMachine.Instance

        0
