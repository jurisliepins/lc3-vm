module LC3VirtualMachine =
    open System
    open System.IO

    exception LC3VirtualMachineException of string

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

    type TrapCodeTypes = 
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

        member public this.RegisterRead(addr: uint16) = (regread this.Registers addr)
        member public this.RegisterWrite(addr: uint16, value: uint16) = (regwrite this.Registers addr value)

        member public this.MemoryRead(addr: uint16) = (memread this.Memory addr)
        member public this.MemoryWrite(addr: uint16, value: uint16) = (memwrite this.Memory addr value)

        member public this.ProgramCounter 
            with get() = this.RegisterRead(uint16 RegisterTypes.R_PC) and set(value: uint16) = this.RegisterWrite(uint16 RegisterTypes.R_PC, value)

        member public this.ProgramCounterWithIncrement with get() = (regreadpcincr this.Registers)
        member public this.ProgramCounterWithDecrement with get() = (regreadpcdecr this.Registers)

        member public this.ConditionFlag 
            with get() = this.RegisterRead(uint16 RegisterTypes.R_COND) and set(value: uint16) = this.RegisterWrite(uint16 RegisterTypes.R_COND, value)

        member public this.UpdateConditionFlags(r: uint16) = 
            match this.RegisterRead(r) with
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
        let instruction = vm.MemoryRead vm.ProgramCounterWithIncrement
        
        match (enum<OpcodeTypes> (int (unpackOp instruction))) with 
        | OP_BR   -> evalOpBr  vm instruction
        | OP_ADD  -> evalOpAdd vm instruction
        | OP_LD   -> evalOpLd  vm instruction
        | OP_ST   -> evalOpSt  vm instruction
        | OP_JSR  -> eval vm
        | OP_AND  -> evalOpAnd vm instruction
        | OP_LDR  -> eval vm
        | OP_STR  -> eval vm
        | OP_RTI  -> eval vm
        | OP_NOT  -> eval vm
        | OP_LDI  -> eval vm
        | OP_STI  -> eval vm
        | OP_JMP  -> eval vm
        | OP_RES  -> eval vm
        | OP_LEA  -> eval vm
        | OP_TRAP -> eval vm
    and evalOpBr (vm: VirtualMachine) (instruction: uint16) =
        let condFlag = unpackDr instruction
        let pcOffset = signExtend (instruction &&& 0x1FFus) 9
        
        if (condFlag &&& vm.ConditionFlag) > 0us then
            vm.ProgramCounter <- (vm.ProgramCounter + pcOffset)
    and evalOpAdd (vm: VirtualMachine) (instruction: uint16) =
        let dr = unpackDr instruction
        let sr1 = unpackSr1 instruction

        let imm = unpackImm instruction

        match imm with 
        | 0us -> 
            let sr2 = unpackSr2 instruction
            vm.RegisterWrite(dr, (vm.RegisterRead(sr1) + vm.RegisterRead(sr2)))
            vm.UpdateConditionFlags(dr)
            eval vm
        | 1us ->
            let immValue = signExtend (instruction &&& 0x1Fus) 5
            vm.RegisterWrite(dr, (vm.RegisterRead(sr1) + immValue))
            vm.UpdateConditionFlags(dr)
            eval vm
        | _ -> 
            raise (LC3VirtualMachineException("Unexpected value for the immediate flag"))
    and evalOpLd (vm: VirtualMachine) (instruction: uint16) =
        let dr = unpackDr instruction
        let pcOffset = signExtend (instruction &&& 0x1FFus) 9
        vm.RegisterWrite(dr, vm.MemoryRead(vm.ProgramCounter + pcOffset))
        vm.UpdateConditionFlags(dr)
        eval vm
    and evalOpSt (vm: VirtualMachine) (instruction: uint16) =
        let sr = unpackDr instruction
        let pcOffset = signExtend (instruction &&& 0x1FFus) 9
        vm.MemoryWrite(vm.ProgramCounter + pcOffset, vm.RegisterRead(sr))
        eval vm
    and evalOpAnd (vm: VirtualMachine) (instruction: uint16) =
        let dr = unpackDr instruction
        let sr1 = unpackSr1 instruction

        let imm = unpackImm instruction

        match imm with 
        | 0us -> 
            let sr2 = unpackSr2 instruction
            vm.RegisterWrite(dr, (vm.RegisterRead(sr1) &&& vm.RegisterRead(sr2)))
            vm.UpdateConditionFlags(dr)
            eval vm
        | 1us ->
            let immValue = signExtend (instruction &&& 0x1Fus) 5
            vm.RegisterWrite(dr, (vm.RegisterRead(sr1) &&& immValue))
            vm.UpdateConditionFlags(dr)
            eval vm
        | _ -> 
            raise (LC3VirtualMachineException("Unexpected value for the immediate flag"))

    [<EntryPoint>]
    let main argv =
        VirtualMachine.Instance.ProgramCounter <- 0x3000us
        VirtualMachine.Instance.Load("C:\\Users\\Yuris Liepins\\Projects\\lc3-vm\\images\\2048.obj")

        eval VirtualMachine.Instance

        0
