namespace LC3VirtualMachine

open System.IO
open System
open LC3VirtualMachine.VirtualMachineException
open LC3VirtualMachine.VirtualMachineTypes

type LC3VirtualMachine() =
    let (memory, registers): VirtualMachine = 
        VirtualMachineTypes.VirtualMachine(
            Array.zeroCreate (int UInt16.MaxValue), 
            Array.zeroCreate (int VirtualMachineTypes.RegisterTypes.R_COUNT))

    static let instance = LC3VirtualMachine()

    static member public Instance with get() = instance

    member public this.VirtualMachine with get(): VirtualMachine = (memory, registers)

    member public this.ReadRegister (addr: uint16) = VirtualRegisters.read registers addr

    member public this.WriteRegister(addr: uint16, value: uint16) = VirtualRegisters.write registers addr value

    member public this.ReadMemory(addr: uint16) = VirtualMemory.read memory addr
    
    member public this.ReadMemoryDirect(addr: uint16) = VirtualMemory.readDirect memory addr

    member public this.WriteMemory(addr: uint16, value: uint16) = VirtualMemory.write memory addr value

    member public this.UpdateConditionFlags(addr: uint16) = VirtualRegisters.updateCondFlags registers addr

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
        with get() = VirtualRegisters.readPc registers and set(value: uint16) = VirtualRegisters.writePc registers value

    member public this.ProgramCounterWithIncrement 
        with get() = VirtualRegisters.readPcWithIncr registers

    member public this.ConditionFlag 
        with get() = VirtualRegisters.readCondFlag registers and set(value: uint16) = VirtualRegisters.writeCondFlag registers value

module OpEvaluator = 
    /// Opcode - Branch
    let inline evalOpBr (vm: LC3VirtualMachine) (instruction: uint16) =
        let cond = Bits.unpack9 instruction
        if (cond &&& vm.ConditionFlag) <> 0us then
            let offset = Bits.signExtend (instruction &&& 0x1FFus) 9
            vm.ProgramCounter <- vm.ProgramCounter + offset

    /// Opcode - Add
    let inline evalOpAdd (vm: LC3VirtualMachine) (instruction: uint16) =
        let r0 = Bits.unpack9 instruction
        let r1 = Bits.unpack6 instruction
        let r1Val = vm.ReadRegister(r1)
        if (Bits.unpackImm instruction) <> 0us then
            let imm = Bits.signExtend (instruction &&& 0x1Fus) 5
            vm.WriteRegister(r0, r1Val + imm)
        else 
            let r2 = Bits.unpack0 instruction
            let r2Val = vm.ReadRegister(r2)
            vm.WriteRegister(r0, r1Val + r2Val)
        vm.UpdateConditionFlags(r0)
    
    /// Opcode - Load
    let inline evalOpLd (vm: LC3VirtualMachine) (instruction: uint16) =
        let r0 = Bits.unpack9 instruction
        let offset = Bits.signExtend (instruction &&& 0x1FFus) 9
        let memVal = vm.ReadMemory(vm.ProgramCounter + offset)
        vm.WriteRegister(r0, memVal)
        vm.UpdateConditionFlags(r0)

    /// Opcode - Store
    let inline evalOpSt (vm: LC3VirtualMachine) (instruction: uint16) =
        let r0 = Bits.unpack9 instruction
        let r0Val = vm.ReadRegister(r0)
        let offset = Bits.signExtend (instruction &&& 0x1FFus) 9
        vm.WriteMemory(vm.ProgramCounter + offset, r0Val)

    /// Opcode - Jump Register
    let inline evalOpJsr (vm: LC3VirtualMachine) (instruction: uint16) =
        vm.R7 <- vm.ProgramCounter
        if (Bits.unpackLong instruction <> 0us) then
            let offset = Bits.signExtend (instruction &&& 0x7FFus) 11
            vm.ProgramCounter <- vm.ProgramCounter + offset
        else 
            let r1 = Bits.unpack6 instruction
            let r1Val = vm.ReadRegister(r1)
            vm.ProgramCounter <- r1Val

    /// Opcode - And
    let inline evalOpAnd (vm: LC3VirtualMachine) (instruction: uint16) =
        let r0 = Bits.unpack9 instruction
        let r1 = Bits.unpack6 instruction
        let r1Val = vm.ReadRegister(r1)
        if (Bits.unpackImm instruction) <> 0us then
            let imm = Bits.signExtend (instruction &&& 0x1Fus) 5
            vm.WriteRegister(r0, r1Val &&& imm)
        else
            let r2 = Bits.unpack0 instruction
            let r2Val = vm.ReadRegister(r2)
            vm.WriteRegister(r0, r1Val &&& r2Val)
        vm.UpdateConditionFlags(r0)

    /// Opcode - Load Register
    let inline evalOpLdr (vm: LC3VirtualMachine) (instruction: uint16) =
        let r0 = Bits.unpack9 instruction
        let r1 = Bits.unpack6 instruction
        let r1Val = vm.ReadRegister(r1)
        let offset = Bits.signExtend (instruction &&& 0x3Fus) 6
        let memVal = vm.ReadMemory(r1Val + offset)
        vm.WriteRegister(r0, memVal)
        vm.UpdateConditionFlags(r0)

    /// Opcode - Unused
    let inline evalOpRti (_: LC3VirtualMachine) (_: uint16) =
        raise (badOpcodeException (OpcodeTypes.OP_RTI.ToString()))

    /// Opcode - Not
    let inline evalOpNot (vm: LC3VirtualMachine) (instruction: uint16) =
        let r0 = Bits.unpack9 instruction
        let r1 = Bits.unpack6 instruction
        let r1Val = vm.ReadRegister(r1)
        vm.WriteRegister(r0, ~~~r1Val)
        vm.UpdateConditionFlags(r0)

    /// Opcode - Store Register
    let inline evalOpStr (vm: LC3VirtualMachine) (instruction: uint16) =
        let r0 = Bits.unpack9 instruction
        let r1 = Bits.unpack6 instruction
        let r0Val = vm.ReadRegister(r0)
        let r1Val = vm.ReadRegister(r1)
        let offset = Bits.signExtend (instruction &&& 0x3Fus) 6
        vm.WriteMemory(r1Val + offset, r0Val)

    /// Opcode - Load Indirect
    let inline evalOpLdi (vm: LC3VirtualMachine) (instruction: uint16) =
        let r0 = Bits.unpack9 instruction
        let offset = Bits.signExtend (instruction &&& 0x1FFus) 9
        let pcVal = vm.ReadMemory(vm.ProgramCounter + offset)
        let memVal = vm.ReadMemory(pcVal)
        vm.WriteRegister(r0, memVal)
        vm.UpdateConditionFlags(r0)

    /// Opcode - Store Indirect
    let inline evalOpSti (vm: LC3VirtualMachine) (instruction: uint16) =
        let r0 = Bits.unpack9 instruction
        let r0Val = vm.ReadRegister(r0)
        let offset = Bits.signExtend (instruction &&& 0x1FFus) 9
        let memVal = vm.ReadMemory(vm.ProgramCounter + offset)
        vm.WriteMemory(memVal, r0Val)

    /// Opcode - Jump
    let inline evalOpJmp (vm: LC3VirtualMachine) (instruction: uint16) =
        let r1 = Bits.unpack6 instruction
        let r1Val = vm.ReadRegister(r1)
        vm.ProgramCounter <- r1Val

    /// Opcode - Unused
    let inline evalOpRes (_: LC3VirtualMachine) (_: uint16) =
        raise (badOpcodeException (OpcodeTypes.OP_RES.ToString()))

    /// Opcode - Load Effective Address
    let inline evalOpLea (vm: LC3VirtualMachine) (instruction: uint16) =
        let r0 = Bits.unpack9 instruction
        let offset = Bits.signExtend (instruction &&& 0x1FFus) 9
        vm.WriteRegister(r0, vm.ProgramCounter + offset)
        vm.UpdateConditionFlags(r0)

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
    let inline evalOpTrapHalt (_: LC3VirtualMachine) (_: uint16) = 
        Console.Out.Flush()

module LC3VirtualMachine =
    let debug (str: string) = Console.Write str
    
    let load (vm: LC3VirtualMachine) (path: string) =       
        use reader = new BinaryReader(File.Open(path, FileMode.Open, FileAccess.Read, FileShare.Read))
        
        let mutable originPtr = Bits.readUInt16 reader
        let mutable memoryPtr = originPtr

        let memory, _ = vm.VirtualMachine;

        while ((reader.BaseStream.Position <> reader.BaseStream.Length) && (memoryPtr < UInt16.MaxValue)) do
            VirtualMemory.write memory memoryPtr (Bits.readUInt16 reader)
            memoryPtr <- memoryPtr + 1us
        
        (originPtr, memoryPtr)

    let rec eval (vm: LC3VirtualMachine) =
        let instruction = vm.ReadMemory(vm.ProgramCounterWithIncrement)

        match (enum<OpcodeTypes> (int (Bits.unpackOp instruction))) with 
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
    and evalOpBr   (vm: LC3VirtualMachine) (instruction: uint16) = OpEvaluator.evalOpBr  vm instruction; eval vm
    and evalOpAdd  (vm: LC3VirtualMachine) (instruction: uint16) = OpEvaluator.evalOpAdd vm instruction; eval vm
    and evalOpLd   (vm: LC3VirtualMachine) (instruction: uint16) = OpEvaluator.evalOpLd  vm instruction; eval vm
    and evalOpSt   (vm: LC3VirtualMachine) (instruction: uint16) = OpEvaluator.evalOpSt  vm instruction; eval vm
    and evalOpJsr  (vm: LC3VirtualMachine) (instruction: uint16) = OpEvaluator.evalOpJsr vm instruction; eval vm
    and evalOpAnd  (vm: LC3VirtualMachine) (instruction: uint16) = OpEvaluator.evalOpAnd vm instruction; eval vm
    and evalOpLdr  (vm: LC3VirtualMachine) (instruction: uint16) = OpEvaluator.evalOpLdr vm instruction; eval vm
    and evalOpRti  (vm: LC3VirtualMachine) (instruction: uint16) = OpEvaluator.evalOpRti vm instruction; eval vm
    and evalOpNot  (vm: LC3VirtualMachine) (instruction: uint16) = OpEvaluator.evalOpNot vm instruction; eval vm
    and evalOpStr  (vm: LC3VirtualMachine) (instruction: uint16) = OpEvaluator.evalOpStr vm instruction; eval vm
    and evalOpLdi  (vm: LC3VirtualMachine) (instruction: uint16) = OpEvaluator.evalOpLdi vm instruction; eval vm
    and evalOpSti  (vm: LC3VirtualMachine) (instruction: uint16) = OpEvaluator.evalOpSti vm instruction; eval vm
    and evalOpJmp  (vm: LC3VirtualMachine) (instruction: uint16) = OpEvaluator.evalOpJmp vm instruction; eval vm
    and evalOpRes  (vm: LC3VirtualMachine) (instruction: uint16) = OpEvaluator.evalOpRes vm instruction; eval vm
    and evalOpLea  (vm: LC3VirtualMachine) (instruction: uint16) = OpEvaluator.evalOpLea vm instruction; eval vm
    and evalOpTrap (vm: LC3VirtualMachine) (instruction: uint16) =
        match (enum<TrapcodeTypes> (int (Bits.unpackTrap instruction))) with 
        | TrapcodeTypes.TRAP_GETC  -> TrapEvaluator.evalOpTrapGetc  vm instruction; eval vm
        | TrapcodeTypes.TRAP_OUT   -> TrapEvaluator.evalOpTrapOut   vm instruction; eval vm
        | TrapcodeTypes.TRAP_PUTS  -> TrapEvaluator.evalOpTrapPuts  vm instruction; eval vm
        | TrapcodeTypes.TRAP_IN    -> TrapEvaluator.evalOpTrapIn    vm instruction; eval vm
        | TrapcodeTypes.TRAP_PUTSP -> TrapEvaluator.evalOpTrapPutsp vm instruction; eval vm
        | TrapcodeTypes.TRAP_HALT  -> TrapEvaluator.evalOpTrapHalt  vm instruction;
        | unknownCode -> 
            raise (unknownTrapcodeException (unknownCode.ToString()))