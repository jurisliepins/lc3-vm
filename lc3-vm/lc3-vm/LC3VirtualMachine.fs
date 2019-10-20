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

    member public this.Load(path: string) =
        use reader = new BinaryReader(File.Open(path, FileMode.Open, FileAccess.Read, FileShare.Read))
            
        let mutable originPtr = LC3Bits.readUInt16 reader
        let mutable memoryPtr = originPtr

        while ((reader.BaseStream.Position <> reader.BaseStream.Length) && (memoryPtr < UInt16.MaxValue)) do
            LC3VirtualMemory.write memory memoryPtr (LC3Bits.readUInt16 reader)
            memoryPtr <- memoryPtr + 1us
        
        (originPtr, memoryPtr)

    member public this.ReadRegister (addr: uint16)                = LC3VirtualRegisters.read  registers addr
    member public this.WriteRegister(addr: uint16, value: uint16) = LC3VirtualRegisters.write registers addr value

    member public this.ReadMemory       (addr: uint16)                = LC3VirtualMemory.read       memory addr
    member public this.ReadMemoryDirect (addr: uint16)                = LC3VirtualMemory.readDirect memory addr
    member public this.WriteMemory      (addr: uint16, value: uint16) = LC3VirtualMemory.write      memory addr value

    member public this.UpdateConditionFlags(addr: uint16) = LC3VirtualRegisters.updateConditionFlag registers addr

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

module TrapEvaluator = 
    let inline evalOpTrapGetc ((_, registers): VirtualMachine) (_: uint16) = 
        //Console.WriteLine("TRAP_GETC")
        LC3VirtualRegisters.write registers (uint16 RegisterTypes.R_R0) (uint16 (Console.ReadKey(true).KeyChar))
        
    let inline evalOpTrapOut ((_, registers): VirtualMachine) (_: uint16) = 
        //Console.WriteLine("TRAP_OUT")
        let c = char (LC3VirtualRegisters.read registers (uint16 RegisterTypes.R_R0))
        Console.Out.Write(c)
        Console.Out.Flush()

    let inline evalOpTrapPuts ((memory, registers): VirtualMachine) (_: uint16) = 
        //Console.WriteLine("TRAP_PUTS")
        let mutable p = LC3VirtualRegisters.read registers (uint16 RegisterTypes.R_R0)
        let mutable c = LC3VirtualMemory.readDirect memory p 
        while c <> 0us do
            Console.Out.Write(char c)
            p <- p + 1us
            c <- LC3VirtualMemory.readDirect memory p 
        Console.Out.Flush()

    let inline evalOpTrapIn ((_, registers): VirtualMachine) (_: uint16) = 
        //Console.WriteLine("TRAP_IN")
        let c = Console.ReadKey(true)
        let r = LC3VirtualRegisters.read registers (uint16 RegisterTypes.R_R0)
        Console.Out.Write(c.KeyChar)
        LC3VirtualRegisters.write registers r (uint16 c.KeyChar)

    let inline evalOpTrapPutsp ((memory, registers): VirtualMachine) (_: uint16) = 
        //Console.WriteLine("TRAP_PUTSP")
        let mutable p = LC3VirtualRegisters.read registers (uint16 RegisterTypes.R_R0)
        let mutable c = LC3VirtualMemory.readDirect memory p 
        while c <> 0us do
            let char1 = c &&& 0xFFus;
            let char2 = c >>> 8;
            Console.Out.Write(char char1)
            Console.Out.Write(char char2)
            p <- p + 1us
            c <- LC3VirtualMemory.readDirect memory p 
        Console.Out.Flush()
    
    let inline evalOpTrapHalt (_: VirtualMachine) (_: uint16) = 
        //Console.WriteLine("TRAP_HALT")
        Console.Out.Flush()

module OpEvaluator = 
    let inline evalOpBr (vm: LC3VirtualMachine) (instruction: uint16) =
        let condFlag = LC3Bits.unpack0 instruction
        let pcOffset = LC3Bits.signExtend (instruction &&& 0x1FFus) 9
        if (condFlag &&& vm.ConditionFlag) <> 0us then
            vm.ProgramCounter <- vm.ProgramCounter + pcOffset

    let inline evalOpAdd (vm: LC3VirtualMachine) (instruction: uint16) =
        let dr = LC3Bits.unpack0 instruction
        match (LC3Bits.unpackImm instruction) with 
        | 0us -> 
            let sr1Value = vm.ReadRegister(LC3Bits.unpack1 instruction)
            let sr2Value = vm.ReadRegister(LC3Bits.unpack2 instruction)
            vm.WriteRegister(dr, sr1Value + sr2Value)
        | _ -> 
            let sr1Value = vm.ReadRegister(LC3Bits.unpack1 instruction)
            let imm = LC3Bits.signExtend (instruction &&& 0x1Fus) 5
            vm.WriteRegister(dr, sr1Value + imm)
        vm.UpdateConditionFlags dr

    let inline evalOpLd (vm: LC3VirtualMachine) (instruction: uint16) =
        let dr = LC3Bits.unpack0 instruction
        let pcOffset = LC3Bits.signExtend (instruction &&& 0x1FFus) 9
        let memValue = vm.ReadMemory(vm.ProgramCounter + pcOffset)
        vm.WriteRegister(dr, memValue)
        vm.UpdateConditionFlags dr

    let inline evalOpSt (vm: LC3VirtualMachine) (instruction: uint16) =
        let sr = LC3Bits.unpack0 instruction
        let pcOffset = LC3Bits.signExtend (instruction &&& 0x1FFus) 9
        let pc = vm.ProgramCounter + pcOffset
        let srValue = vm.ReadRegister(sr)
        vm.WriteMemory(pc, srValue)

    let inline evalOpJsr (vm: LC3VirtualMachine) (instruction: uint16) =
        let sr = LC3Bits.unpack1 instruction
        let longPcOffset = LC3Bits.signExtend (instruction &&& 0x7FFus) 11
        let pc = vm.ProgramCounter
        vm.WriteRegister(uint16 RegisterTypes.R_R7, pc)
        match LC3Bits.unpackLong instruction with
        | 0us -> vm.ProgramCounter <- vm.ReadRegister(sr)
        | _   -> vm.ProgramCounter <- pc + longPcOffset

    let inline evalOpAnd (vm: LC3VirtualMachine) (instruction: uint16) =
        let dr = LC3Bits.unpack0 instruction
        match LC3Bits.unpackImm instruction with 
        | 0us -> 
            let sr1 = LC3Bits.unpack1 instruction
            let sr2 = LC3Bits.unpack2 instruction
            let sr1Value = vm.ReadRegister(sr1)
            let sr2Value = vm.ReadRegister(sr2)
            vm.WriteRegister(dr, sr1Value &&& sr2Value)
        | _ -> 
            let sr1 = LC3Bits.unpack1 instruction
            let imm = LC3Bits.signExtend (instruction &&& 0x1Fus) 5
            let sr1Value = vm.ReadRegister(sr1)
            vm.WriteRegister(dr, sr1Value &&& imm)
        vm.UpdateConditionFlags dr

    let inline evalOpLdr (vm: LC3VirtualMachine) (instruction: uint16) =
        let dr = LC3Bits.unpack0 instruction
        let sr = LC3Bits.unpack1 instruction
        let pcOffset = LC3Bits.signExtend (instruction &&& 0x3Fus) 6
        let srValue = vm.ReadRegister(sr) + pcOffset
        let memValue = vm.ReadMemory(srValue)
        vm.WriteRegister(dr, memValue)
        vm.UpdateConditionFlags dr

    let inline evalOpRti (_: LC3VirtualMachine) (_: uint16) =
        raise (badOpcodeException (OpcodeTypes.OP_RTI.ToString()))

    let inline evalOpNot (vm: LC3VirtualMachine) (instruction: uint16) =
        let dr = LC3Bits.unpack0 instruction
        let sr = LC3Bits.unpack1 instruction
        let srValue = vm.ReadRegister(sr)
        vm.WriteRegister(dr, srValue)
        vm.UpdateConditionFlags dr

    let inline evalOpStr (vm: LC3VirtualMachine) (instruction: uint16) =
        let sr1 = LC3Bits.unpack0 instruction
        let sr2 = LC3Bits.unpack1 instruction
        let offset = LC3Bits.signExtend (instruction &&& 0x3Fus) 6
        let sr1Value = vm.ReadRegister(sr1)
        let sr2Value = vm.ReadRegister(sr2)
        vm.WriteMemory(sr2Value + offset, sr1Value)

    let inline evalOpLdi (vm: LC3VirtualMachine) (instruction: uint16) =
        let dr = LC3Bits.unpack0 instruction
        let pcOffset = LC3Bits.signExtend (instruction &&& 0x1FFus) 9
        let pc = vm.ProgramCounter
        let pcValue = vm.ReadMemory(pc + pcOffset)
        let memValue = vm.ReadMemory(pcValue)
        vm.WriteRegister(dr, memValue)
        vm.UpdateConditionFlags dr

    let inline evalOpSti (vm: LC3VirtualMachine) (instruction: uint16) =
        let dr = LC3Bits.unpack0 instruction
        let pcOffset = LC3Bits.signExtend (instruction &&& 0x1FFus) 9
        let drValue = vm.ReadRegister(dr)
        let memValue = vm.ReadMemory(vm.ProgramCounter + pcOffset)
        vm.WriteMemory(memValue, drValue)

    let inline evalOpJmp (vm: LC3VirtualMachine) (instruction: uint16) =
        let sr = LC3Bits.unpack1 instruction
        let srValue = vm.ReadRegister(sr)
        vm.ProgramCounter <- srValue

    let inline evalOpRes (_: LC3VirtualMachine) (_: uint16) =
        raise (badOpcodeException (OpcodeTypes.OP_RES.ToString()))

    let inline evalOpLea (vm: LC3VirtualMachine) (instruction: uint16) =
        let dr = LC3Bits.unpack0 instruction
        let pc = vm.ProgramCounter
        let pcOffset = LC3Bits.signExtend (instruction &&& 0x1FFus) 9
        
        vm.WriteRegister(dr, pc + pcOffset)
        vm.UpdateConditionFlags dr

module LC3VirtualMachine =
    let dumpMemory         ((memory, _):    VirtualMachine) = LC3VirtualMemory.dump         memory
    let dumpNonEmptyMemory ((memory, _):    VirtualMachine) = LC3VirtualMemory.dumpNonEmpty memory
    let dumpRegisters      ((_, registers): VirtualMachine) = LC3VirtualRegisters.dump      registers

    let rec eval (vm: LC3VirtualMachine) =
        let instruction = vm.ReadMemory(vm.ProgramCounterWithIncrement)

        match (enum<OpcodeTypes> (int (LC3Bits.unpackOp instruction))) with 
        | OpcodeTypes.OP_BR   -> (*Console.WriteLine("OP_BR");*)   evalOpBr   vm instruction
        | OpcodeTypes.OP_ADD  -> (*Console.WriteLine("OP_ADD");*)  evalOpAdd  vm instruction
        | OpcodeTypes.OP_LD   -> (*Console.WriteLine("OP_LD");*)   evalOpLd   vm instruction
        | OpcodeTypes.OP_ST   -> (**)Console.WriteLine("OP_ST");   evalOpSt   vm instruction // Unchecked!
        | OpcodeTypes.OP_JSR  -> (*Console.WriteLine("OP_JSR");*)  evalOpJsr  vm instruction
        | OpcodeTypes.OP_AND  -> (*Console.WriteLine("OP_AND");*)  evalOpAnd  vm instruction
        | OpcodeTypes.OP_LDR  -> (*Console.WriteLine("OP_LDR");*)  evalOpLdr  vm instruction
        | OpcodeTypes.OP_STR  -> (*Console.WriteLine("OP_STR");*)  evalOpStr  vm instruction
        | OpcodeTypes.OP_RTI  -> (**)Console.WriteLine("OP_RTI");  evalOpRti  vm instruction // Unchecked!
        | OpcodeTypes.OP_NOT  -> (**)Console.WriteLine("OP_NOT");  evalOpNot  vm instruction // Unchecked!
        | OpcodeTypes.OP_LDI  -> (*Console.WriteLine("OP_LDI");*)  evalOpLdi  vm instruction
        | OpcodeTypes.OP_STI  -> (**)Console.WriteLine("OP_STI");  evalOpSti  vm instruction // Unchecked!
        | OpcodeTypes.OP_JMP  -> (**)Console.WriteLine("OP_JMP");  evalOpJmp  vm instruction // Unchecked!
        | OpcodeTypes.OP_RES  -> (**)Console.WriteLine("OP_RES");  evalOpRes  vm instruction // Unchecked!
        | OpcodeTypes.OP_LEA  -> (*Console.WriteLine("OP_LEA");*)  evalOpLea  vm instruction
        | OpcodeTypes.OP_TRAP -> (*Console.WriteLine("OP_TRAP");*) evalOpTrap vm instruction
        | unknownCode ->
            raise (unknownOpcodeException (unknownCode.ToString()))
    and evalOpBr (vm: LC3VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpBr vm instruction
        //
        //dumpNonEmptyMemory vm 0us
        //dumpRegisters vm
        //
        eval vm
    and evalOpAdd (vm: LC3VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpAdd vm instruction
        eval vm
    and evalOpLd (vm: LC3VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpLd vm instruction
        eval vm
    and evalOpSt (vm: LC3VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpSt vm instruction
        eval vm
    and evalOpJsr (vm: LC3VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpJsr vm instruction
        eval vm
    and evalOpAnd (vm: LC3VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpAnd vm instruction
        eval vm
    and evalOpLdr (vm: LC3VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpLdr vm instruction
        eval vm
    and evalOpRti (vm: LC3VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpRti vm instruction
        eval vm
    and evalOpNot (vm: LC3VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpNot vm instruction
        eval vm
    and evalOpStr (vm: LC3VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpStr vm instruction
        eval vm
    and evalOpLdi (vm: LC3VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpLdi vm instruction
        eval vm
    and evalOpSti (vm: LC3VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpSti vm instruction
        eval vm
    and evalOpJmp (vm: LC3VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpJmp vm instruction
        eval vm
    and evalOpRes (vm: LC3VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpRes vm instruction
        eval vm
    and evalOpLea (vm: LC3VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpLea vm instruction
        eval vm
    and evalOpTrap (vm: LC3VirtualMachine) (instruction: uint16) =
        match (enum<TrapcodeTypes> (int (LC3Bits.unpackTrap instruction))) with 
        | TrapcodeTypes.TRAP_GETC -> 
            TrapEvaluator.evalOpTrapGetc vm.VirtualMachine instruction
            eval vm
        | TrapcodeTypes.TRAP_OUT -> 
            TrapEvaluator.evalOpTrapOut vm.VirtualMachine instruction
            eval vm
        | TrapcodeTypes.TRAP_PUTS -> 
            TrapEvaluator.evalOpTrapPuts vm.VirtualMachine instruction
            eval vm
        | TrapcodeTypes.TRAP_IN -> 
            TrapEvaluator.evalOpTrapIn vm.VirtualMachine instruction
            eval vm
        | TrapcodeTypes.TRAP_PUTSP -> 
            TrapEvaluator.evalOpTrapPutsp vm.VirtualMachine instruction
            eval vm
        | TrapcodeTypes.TRAP_HALT -> 
            TrapEvaluator.evalOpTrapHalt  vm.VirtualMachine instruction
        | unknownCode -> 
            raise (unknownTrapcodeException (unknownCode.ToString()))