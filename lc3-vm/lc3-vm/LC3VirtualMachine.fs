namespace lc3vm

open System.IO
open System
open lc3vm.LC3VirtualMachineException
open lc3vm.LC3VirtualMachineTypes

type LC3VirtualMachine() =
    let (memory, registers): VirtualMachine = 
        LC3VirtualMachineTypes.VirtualMachine(
            Array.zeroCreate (int UInt16.MaxValue), 
            Array.zeroCreate (int LC3VirtualMachineTypes.RegisterTypes.R_COUNT))

    static let instance = LC3VirtualMachine()

    static member public Instance with get() = instance

    member public this.VirtualMachine with get(): VirtualMachine = (memory, registers)

    member public this.ReadRegister (addr: uint16) = LC3VirtualRegisters.read registers addr

    member public this.WriteRegister(addr: uint16, value: uint16) = LC3VirtualRegisters.write registers addr value

    member public this.ReadMemory(addr: uint16) = LC3VirtualMemory.read memory addr
    
    member public this.ReadMemoryDirect(addr: uint16) = LC3VirtualMemory.readDirect memory addr

    member public this.WriteMemory(addr: uint16, value: uint16) = LC3VirtualMemory.write memory addr value

    member public this.UpdateConditionFlags(addr: uint16) = LC3VirtualRegisters.updateConditionFlags registers addr

    member public this.R0
        with get() = this.ReadRegister(uint16 RegisterTypes.R_R0) and set(value: uint16) = this.WriteRegister(uint16 RegisterTypes.R_R0, value)

    member public this.R1
        with get() = this.ReadRegister(uint16 RegisterTypes.R_R1) and set(value: uint16) = this.WriteRegister(uint16 RegisterTypes.R_R1, value)

    member public this.R2
        with get() = this.ReadRegister(uint16 RegisterTypes.R_R2) and set(value: uint16) = this.WriteRegister(uint16 RegisterTypes.R_R2, value)

    member public this.R3
        with get() = this.ReadRegister(uint16 RegisterTypes.R_R3) and set(value: uint16) = this.WriteRegister(uint16 RegisterTypes.R_R3, value)

    member public this.R4
        with get() = this.ReadRegister(uint16 RegisterTypes.R_R4) and set(value: uint16) = this.WriteRegister(uint16 RegisterTypes.R_R4, value)

    member public this.R5
        with get() = this.ReadRegister(uint16 RegisterTypes.R_R5) and set(value: uint16) = this.WriteRegister(uint16 RegisterTypes.R_R5, value)

    member public this.R6
        with get() = this.ReadRegister(uint16 RegisterTypes.R_R6) and set(value: uint16) = this.WriteRegister(uint16 RegisterTypes.R_R6, value)

    member public this.R7
        with get() = this.ReadRegister(uint16 RegisterTypes.R_R7) and set(value: uint16) = this.WriteRegister(uint16 RegisterTypes.R_R7, value)

    member public this.ProgramCounter 
        with get() = LC3VirtualRegisters.readProgramCounter registers and set(value: uint16) = LC3VirtualRegisters.writeProgramCounter registers value

    member public this.ProgramCounterWithIncrement 
        with get() = LC3VirtualRegisters.readProgramCounterWithIncerement registers

    member public this.ConditionFlag 
        with get() = LC3VirtualRegisters.readConditionFlag registers and set(value: uint16) = LC3VirtualRegisters.writeConditionFlag registers value

    member public this.IsProgramCounterEnd
        with get() = this.ProgramCounter = uint16 memory.Length

module TrapEvaluator = 
    /// Trapcode - Read Single Char
    let inline evalOpTrapGetc (vm: LC3VirtualMachine) (_: uint16) = 
        vm.R0 <- uint16 (Console.ReadKey(true).KeyChar)
        
    /// Trapcode - Write Char To Stdout
    let inline evalOpTrapOut (vm: LC3VirtualMachine) (_: uint16) = 
        Console.Out.Write(char vm.R0)
        Console.Out.Flush()
    
    /// Trapcode - Write String To Stdout
    let inline evalOpTrapPuts (vm: LC3VirtualMachine) (_: uint16) = 
        let mutable p = vm.R0
        let mutable c = vm.ReadMemoryDirect(p)
        while c <> 0us do
            Console.Out.Write(char c)
            p <- p + 1us
            c <- vm.ReadMemoryDirect(p)
        Console.Out.Flush()

    /// Trapcode - Get Single Char And Echo
    let inline evalOpTrapIn (vm: LC3VirtualMachine) (_: uint16) = 
        let c = Console.ReadKey(true).KeyChar
        Console.Out.Write(c)
        vm.R0 <- uint16 c
    
    /// Trapcode - Write Multiple Chars To Stdout
    let inline evalOpTrapPutsp (vm: LC3VirtualMachine) (_: uint16) = 
        let mutable p = vm.R0
        let mutable c = vm.ReadMemoryDirect(p)
        while c <> 0us do
            let char1 = c &&& 0xFFus;
            let char2 = c >>> 8;
            Console.Out.Write(char char1)
            Console.Out.Write(char char2)
            p <- p + 1us
            c <- vm.ReadMemoryDirect(p)
        Console.Out.Flush()
    
    /// Trapcode - Halt Program
    let inline evalOpTrapHalt (_: VirtualMachine) (_: uint16) = 
        Console.Out.Flush()

module OpEvaluator = 
    /// Opcode - Branch
    let inline evalOpBr (vm: LC3VirtualMachine) (instruction: uint16) =
        let condFlag = LC3Bits.unpack9 instruction
        if (condFlag &&& vm.ConditionFlag) <> 0us then
            let offset = LC3Bits.signExtend (instruction &&& 0x1FFus) 9
            vm.ProgramCounter <- vm.ProgramCounter + offset

    /// Opcode - Add
    let inline evalOpAdd (vm: LC3VirtualMachine) (instruction: uint16) =
        let r0 = LC3Bits.unpack9 instruction
        let r1 = LC3Bits.unpack6 instruction
        let r1Value = vm.ReadRegister(r1)
        if (LC3Bits.unpackImm instruction) <> 0us then
            let immValue = LC3Bits.signExtend (uint16 (int instruction &&& 0x1F)) 5
            vm.WriteRegister(r0, r1Value + immValue)
        else 
            let r2 = LC3Bits.unpack0 instruction
            let r2Value = vm.ReadRegister(r2)
            vm.WriteRegister(r0, r1Value + r2Value)
        vm.UpdateConditionFlags(r0)
    
    /// Opcode - Load
    let inline evalOpLd (vm: LC3VirtualMachine) (instruction: uint16) =
        let r0 = LC3Bits.unpack9 instruction
        let offset = LC3Bits.signExtend (uint16 (int instruction &&& 0x1FF)) 9
        vm.WriteRegister(r0, vm.ReadMemory(vm.ProgramCounter + offset))
        vm.UpdateConditionFlags(r0)

    /// Opcode - Store
    let inline evalOpSt (vm: LC3VirtualMachine) (instruction: uint16) =
        let r0 = LC3Bits.unpack9 instruction
        let r0Value = vm.ReadRegister(r0)
        let offset = LC3Bits.signExtend (uint16 (int instruction &&& 0x1FF)) 9
        vm.WriteMemory(vm.ProgramCounter + offset, r0Value)

    /// Opcode - Jump Register
    let inline evalOpJsr (vm: LC3VirtualMachine) (instruction: uint16) =
        vm.R7 <- vm.ProgramCounter

        if (LC3Bits.unpackLong instruction <> 0us) then
            let offset = LC3Bits.signExtend (uint16 (int instruction &&& 0x7FF)) 11
            vm.ProgramCounter <- vm.ProgramCounter + offset
        else 
            let r1 = LC3Bits.unpack6 instruction
            vm.ProgramCounter <- vm.ReadRegister(r1)

    /// Opcode - And
    let inline evalOpAnd (vm: LC3VirtualMachine) (instruction: uint16) =
        let r0 = LC3Bits.unpack9 instruction
        let r1 = LC3Bits.unpack6 instruction
        let r1Value = vm.ReadRegister(r1)
        if (LC3Bits.unpackImm instruction) <> 0us then
            let immValue = LC3Bits.signExtend (instruction &&& 0x1Fus) 5
            vm.WriteRegister(r0, r1Value &&& immValue)
        else
            let r2 = LC3Bits.unpack0 instruction
            let r2Value = vm.ReadRegister(r2)
            vm.WriteRegister(r0, r1Value &&& r2Value)
        vm.UpdateConditionFlags(r0)

    /// Opcode - Load Register
    let inline evalOpLdr (vm: LC3VirtualMachine) (instruction: uint16) =
        let r0 = LC3Bits.unpack9 instruction
        let r1 = LC3Bits.unpack6 instruction
        let r1Value = vm.ReadRegister(r1)
        let offset = LC3Bits.signExtend (uint16 (int instruction &&& 0x3F)) 6
        vm.WriteRegister(r0, vm.ReadMemory(r1Value + offset))
        vm.UpdateConditionFlags(r0)

    /// Opcode - Unused
    let inline evalOpRti (_: LC3VirtualMachine) (_: uint16) =
        raise (badOpcodeException (OpcodeTypes.OP_RTI.ToString()))

    /// Opcode - Not
    let inline evalOpNot (vm: LC3VirtualMachine) (instruction: uint16) =
        let r0 = LC3Bits.unpack9 instruction
        let r1 = LC3Bits.unpack6 instruction
        let r1Value = vm.ReadRegister(r1)
        vm.WriteRegister(r0, ~~~r1Value)
        vm.UpdateConditionFlags(r0)

    /// Opcode - Store Register
    let inline evalOpStr (vm: LC3VirtualMachine) (instruction: uint16) =
        let r0 = LC3Bits.unpack9 instruction
        let r1 = LC3Bits.unpack6 instruction
        let r0Value = vm.ReadRegister(r0)
        let r1Value = vm.ReadRegister(r1)
        let offset = LC3Bits.signExtend (uint16 (int instruction &&& 0x3F)) 6
        vm.WriteMemory(r1Value + offset, r0Value)

    /// Opcode - Load Indirect
    let inline evalOpLdi (vm: LC3VirtualMachine) (instruction: uint16) =
        let r0 = LC3Bits.unpack9 instruction
        let offset = LC3Bits.signExtend (uint16 (int instruction &&& 0x1FF)) 9
        let pcValue = vm.ReadMemory(vm.ProgramCounter + offset)
        let memValue = vm.ReadMemory(pcValue)
        vm.WriteRegister(r0, memValue)
        vm.UpdateConditionFlags(r0)

    /// Opcode - Store Indirect
    let inline evalOpSti (vm: LC3VirtualMachine) (instruction: uint16) =
        let r0 = LC3Bits.unpack9 instruction
        let r0Value = vm.ReadRegister(r0)
        let offset = LC3Bits.signExtend (uint16 (int instruction &&& 0x1FF)) 9
        vm.WriteMemory(vm.ReadMemory(vm.ProgramCounter + offset), r0Value)

    /// Opcode - Jump
    let inline evalOpJmp (vm: LC3VirtualMachine) (instruction: uint16) =
        let r1 = LC3Bits.unpack6 instruction
        let r1Value = vm.ReadRegister(r1)
        vm.ProgramCounter <- r1Value

    /// Opcode - Unused
    let inline evalOpRes (_: LC3VirtualMachine) (_: uint16) =
        raise (badOpcodeException (OpcodeTypes.OP_RES.ToString()))

    /// Opcode - Load Effective Address
    let inline evalOpLea (vm: LC3VirtualMachine) (instruction: uint16) =
        let r0 = LC3Bits.unpack9 instruction
        let offset = LC3Bits.signExtend (uint16 (int instruction &&& 0x1FF)) 9
        vm.WriteRegister(r0, vm.ProgramCounter + offset)
        vm.UpdateConditionFlags(r0)

module LC3VirtualMachine =
    (*
    let debug (str: string) = Console.Write(str)

    let dumpMemory ((memory, _): VirtualMachine) (lower: uint16) (upper: uint16) = LC3VirtualMemory.dump memory lower upper debug
    
    let dumpNonEmptyMemory ((memory, _): VirtualMachine) = LC3VirtualMemory.dumpNonEmpty memory 0us debug

    let dumpRegisters ((_, registers): VirtualMachine) = LC3VirtualRegisters.dump registers debug
    *)
    let load (vm: LC3VirtualMachine) (path: string) =       
        use reader = new BinaryReader(File.Open(path, FileMode.Open, FileAccess.Read, FileShare.Read))
        
        let mutable originPtr = LC3Bits.readUInt16 reader
        let mutable memoryPtr = originPtr

        let memory, _ = vm.VirtualMachine;

        while ((reader.BaseStream.Position <> reader.BaseStream.Length) && (memoryPtr < UInt16.MaxValue)) do
            LC3VirtualMemory.write memory memoryPtr (LC3Bits.readUInt16 reader)
            memoryPtr <- memoryPtr + 1us
        
        (originPtr, memoryPtr)

    let rec eval (vm: LC3VirtualMachine) =
        let instruction = vm.ReadMemory(vm.ProgramCounterWithIncrement)

        match (enum<OpcodeTypes> (int (LC3Bits.unpackOp instruction))) with 
        | OpcodeTypes.OP_BR   -> (*debug ("OP_BR  \n");*) evalOpBr  vm instruction 
        | OpcodeTypes.OP_ADD  -> (*debug ("OP_ADD \n");*) evalOpAdd vm instruction
        | OpcodeTypes.OP_LD   -> (*debug ("OP_LD  \n");*) evalOpLd  vm instruction
        | OpcodeTypes.OP_ST   -> (*debug ("OP_ST  \n");*) evalOpSt  vm instruction
        | OpcodeTypes.OP_JSR  -> (*debug ("OP_JSR \n");*) evalOpJsr vm instruction
        | OpcodeTypes.OP_AND  -> (*debug ("OP_AND \n");*) evalOpAnd vm instruction
        | OpcodeTypes.OP_LDR  -> (*debug ("OP_LDR \n");*) evalOpLdr vm instruction
        | OpcodeTypes.OP_STR  -> (*debug ("OP_STR \n");*) evalOpStr vm instruction
        | OpcodeTypes.OP_RTI  -> (*debug ("OP_RTI \n");*) evalOpRti vm instruction
        | OpcodeTypes.OP_NOT  -> (*debug ("OP_NOT \n");*) evalOpNot vm instruction
        | OpcodeTypes.OP_LDI  -> (*debug ("OP_LDI \n");*) evalOpLdi vm instruction
        | OpcodeTypes.OP_STI  -> (*debug ("OP_STI \n");*) evalOpSti vm instruction
        | OpcodeTypes.OP_JMP  -> (*debug ("OP_JMP \n");*) evalOpJmp vm instruction
        | OpcodeTypes.OP_RES  -> (*debug ("OP_RES \n");*) evalOpRes vm instruction
        | OpcodeTypes.OP_LEA  -> (*debug ("OP_LEA \n");*) evalOpLea vm instruction
        | OpcodeTypes.OP_TRAP -> evalOpTrap vm instruction
        | unknownCode ->
            raise (unknownOpcodeException (unknownCode.ToString()))
    and evalOpBr (vm: LC3VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpBr vm instruction
        //dumpNonEmptyMemory vm.VirtualMachine
        //dumpRegisters vm.VirtualMachine
        eval vm
    and evalOpAdd (vm: LC3VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpAdd vm instruction
        //dumpNonEmptyMemory vm.VirtualMachine
        //dumpRegisters vm.VirtualMachine
        eval vm
    and evalOpLd (vm: LC3VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpLd vm instruction
        //dumpNonEmptyMemory vm.VirtualMachine
        //dumpRegisters vm.VirtualMachine
        eval vm
    and evalOpSt (vm: LC3VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpSt vm instruction
        //dumpNonEmptyMemory vm.VirtualMachine
        //dumpRegisters vm.VirtualMachine
        eval vm
    and evalOpJsr (vm: LC3VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpJsr vm instruction
        //dumpNonEmptyMemory vm.VirtualMachine
        //dumpRegisters vm.VirtualMachine
        eval vm
    and evalOpAnd (vm: LC3VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpAnd vm instruction
        //dumpNonEmptyMemory vm.VirtualMachine
        //dumpRegisters vm.VirtualMachine
        eval vm
    and evalOpLdr (vm: LC3VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpLdr vm instruction
        //dumpNonEmptyMemory vm.VirtualMachine
        //dumpRegisters vm.VirtualMachine
        eval vm
    and evalOpRti (vm: LC3VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpRti vm instruction
        //dumpNonEmptyMemory vm.VirtualMachine
        //dumpRegisters vm.VirtualMachine
        eval vm
    and evalOpNot (vm: LC3VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpNot vm instruction
        //dumpNonEmptyMemory vm.VirtualMachine
        //dumpRegisters vm.VirtualMachine
        eval vm
    and evalOpStr (vm: LC3VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpStr vm instruction
        //dumpNonEmptyMemory vm.VirtualMachine
        //dumpRegisters vm.VirtualMachine
        eval vm
    and evalOpLdi (vm: LC3VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpLdi vm instruction
        //dumpNonEmptyMemory vm.VirtualMachine
        //dumpRegisters vm.VirtualMachine
        eval vm
    and evalOpSti (vm: LC3VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpSti vm instruction
        //dumpNonEmptyMemory vm.VirtualMachine
        //dumpRegisters vm.VirtualMachine
        eval vm
    and evalOpJmp (vm: LC3VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpJmp vm instruction
        //dumpNonEmptyMemory vm.VirtualMachine
        //dumpRegisters vm.VirtualMachine
        eval vm
    and evalOpRes (vm: LC3VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpRes vm instruction
        //dumpNonEmptyMemory vm.VirtualMachine
        //dumpRegisters vm.VirtualMachine
        eval vm
    and evalOpLea (vm: LC3VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpLea vm instruction
        //dumpNonEmptyMemory vm.VirtualMachine
        //dumpRegisters vm.VirtualMachine
        eval vm
    and evalOpTrap (vm: LC3VirtualMachine) (instruction: uint16) =
        match (enum<TrapcodeTypes> (int (LC3Bits.unpackTrap instruction))) with 
        | TrapcodeTypes.TRAP_GETC -> 
            //debug ("TRAP_GETC \n")
            TrapEvaluator.evalOpTrapGetc vm instruction
            //dumpNonEmptyMemory vm.VirtualMachine
            //dumpRegisters vm.VirtualMachine
            eval vm
        | TrapcodeTypes.TRAP_OUT -> 
            //debug ("TRAP_OUT  \n")
            TrapEvaluator.evalOpTrapOut vm instruction
            //dumpNonEmptyMemory vm.VirtualMachine
            //dumpRegisters vm.VirtualMachine
            eval vm
        | TrapcodeTypes.TRAP_PUTS -> 
            //debug ("TRAP_PUTS \n")
            TrapEvaluator.evalOpTrapPuts vm instruction
            //dumpNonEmptyMemory vm.VirtualMachine
            //dumpRegisters vm.VirtualMachine
            eval vm
        | TrapcodeTypes.TRAP_IN -> 
            //debug ("TRAP_IN  \n")
            TrapEvaluator.evalOpTrapIn vm instruction
            //dumpNonEmptyMemory vm.VirtualMachine
            //dumpRegisters vm.VirtualMachine
            eval vm
        | TrapcodeTypes.TRAP_PUTSP -> 
            //debug ("TRAP_PUTSP\n")
            TrapEvaluator.evalOpTrapPutsp vm instruction
            //dumpNonEmptyMemory vm.VirtualMachine
            //dumpRegisters vm.VirtualMachine
            eval vm
        | TrapcodeTypes.TRAP_HALT -> 
            //debug ("TRAP_HALT\n")
            TrapEvaluator.evalOpTrapHalt vm.VirtualMachine instruction
            //dumpNonEmptyMemory vm.VirtualMachine
            //dumpRegisters vm.VirtualMachine
        | unknownCode -> 
            raise (unknownTrapcodeException (unknownCode.ToString()))