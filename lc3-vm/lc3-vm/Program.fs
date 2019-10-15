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
        | R_COUNT = 9

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

    //
    type Memory = array<uint16>

    let memread (memory: Memory) (addr: uint16) = 
        memory.[int addr]

    let memwrite (memory: Memory) (addr: uint16) (value: uint16) = 
        memory.[int addr] <- value
    
    //
    type Registers = array<uint16>

    let getpc (registers: Registers) = 
        registers.[int RegisterTypes.R_PC]

    let setpc (registers: Registers) (value: uint16) = 
        registers.[int RegisterTypes.R_PC] <- value

    let getpcincr (registers: Registers) =
        let value = getpc registers
        setpc registers (value + 1us)
        value

    let getpcdecr (registers: Registers) =
        let value = getpc registers
        setpc registers (value - 1us)
        value

    type VirtualMachine () = 
        let memory: Memory = Array.zeroCreate (int UInt16.MaxValue)
        let registers: Registers = Array.zeroCreate (int RegisterTypes.R_COUNT)

        static let instance = VirtualMachine()

        static member public Instance with get() = instance

        member public this.Memory with get() = memory
        member public this.Registers with get() = registers

        member public this.MemoryRead(addr: uint16) = (memread this.Memory addr)
        member public this.MemoryWrite(addr: uint16, value: uint16) = (memwrite this.Memory addr value)

        member public this.ProgramCounter 
            with get() = (getpc this.Registers) and set(value: uint16) = (setpc this.Registers value)
        
        member public this.ProgramCounterWithIncrement with get() = (getpcincr this.Registers)
        member public this.ProgramCounterWithDecrement with get() = (getpcdecr this.Registers)

        member public this.GetRegister(addr: uint16) = registers.[int addr]
        member public this.SetRegister(addr: uint16, value: uint16) = registers.[int addr] <- value

        member public this.Load(filename: string) =
            use reader = new BinaryReader(File.Open(filename, FileMode.Open, FileAccess.Read, FileShare.Read))
            
            let mutable memoryPtr: uint16 = readUInt16 reader

            while ((reader.BaseStream.Position <> reader.BaseStream.Length) && (memoryPtr < UInt16.MaxValue)) do
                this.Memory.[int memoryPtr] <- readUInt16 reader
                memoryPtr <- memoryPtr + 1us
    //
    //let rec evalLoop() =
    //    let instruction = VirtualMachine.Instance.MemoryRead VirtualMachine.Instance.ProgramCounterWithIncrement
        
    //    match (enum<OpcodeTypes> (int (unpackOp instruction))) with 
    //    | OP_BR   -> evalLoop ()
    //    | OP_ADD  -> evalAnd instruction
    //    | OP_LD   -> evalLoop ()
    //    | OP_ST   -> evalLoop ()
    //    | OP_JSR  -> evalLoop ()
    //    | OP_AND  -> evalLoop ()
    //    | OP_LDR  -> evalLoop ()
    //    | OP_STR  -> evalLoop ()
    //    | OP_RTI  -> evalLoop ()
    //    | OP_NOT  -> evalLoop ()
    //    | OP_LDI  -> evalLoop ()
    //    | OP_STI  -> evalLoop ()
    //    | OP_JMP  -> evalLoop ()
    //    | OP_RES  -> evalLoop ()
    //    | OP_LEA  -> evalLoop ()
    //    | OP_TRAP -> evalLoop ()
    //and evalAnd (instruction: uint16) =
    //    let dr = unpackDr instruction
    //    let sr1 = unpackSr1 instruction

    //    let imm = unpackImm instruction

    //    match imm with 
    //    | 0us -> 
    //        let sr2 = unpackSr2 instruction
    //        VirtualMachine.Instance.SetRegister(
    //            dr, (VirtualMachine.Instance.GetRegister(sr1) + VirtualMachine.Instance.GetRegister(sr2)))
    //    | 1us ->
    //        let immValue = signExtend (instruction &&& 0x1Fus) 5
    //        VirtualMachine.Instance.SetRegister(
    //            dr, (VirtualMachine.Instance.GetRegister(sr1) + immValue))
    //    | _ -> 
    //        raise (LC3VirtualMachineException("Unexpected value for the immediate flag"))

    [<EntryPoint>]
    let main argv =
        VirtualMachine.Instance.ProgramCounter <- 0x3000us
        VirtualMachine.Instance.Load("C:\\Users\\Yuris Liepins\\Projects\\lc3-vm\\images\\2048.obj")

        0
