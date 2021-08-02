namespace LC3VirtualMachine

open System
open System.IO
open LC3VirtualMachine
open EvaluatorExceptions
open VirtualMachineTypes

module OpCodeEvaluator = 
    /// OpCode - Branch
    let evalOpBr (vm: VirtualMachine) (instruction: uint16): unit =
        let cond = Bits.unpack9 instruction
        if (cond &&& vm.ConditionFlag) <> 0us then
            let offset = Bits.signExtend (instruction &&& 0x1FFus) 9
            vm.ProgramCounter <- vm.ProgramCounter + offset

    /// OpCode - Add
    let evalOpAdd (vm: VirtualMachine) (instruction: uint16): unit =
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
    
    /// OpCode - Load
    let evalOpLd (vm: VirtualMachine) (instruction: uint16): unit =
        let r0 = Bits.unpack9 instruction
        let offset = Bits.signExtend (instruction &&& 0x1FFus) 9
        let memVal = vm.ReadMemoryInstruction(vm.ProgramCounter + offset)
        vm.WriteRegister(r0, memVal)
        vm.UpdateConditionFlags(r0)

    /// OpCode - Store
    let evalOpSt (vm: VirtualMachine) (instruction: uint16): unit =
        let r0 = Bits.unpack9 instruction
        let r0Val = vm.ReadRegister(r0)
        let offset = Bits.signExtend (instruction &&& 0x1FFus) 9
        vm.WriteMemory(vm.ProgramCounter + offset, r0Val)

    /// OpCode - Jump Register
    let evalOpJsr (vm: VirtualMachine) (instruction: uint16): unit =
        vm.R7 <- vm.ProgramCounter
        if (Bits.unpackLong instruction <> 0us) then
            let offset = Bits.signExtend (instruction &&& 0x7FFus) 11
            vm.ProgramCounter <- vm.ProgramCounter + offset
        else 
            let r1 = Bits.unpack6 instruction
            let r1Val = vm.ReadRegister(r1)
            vm.ProgramCounter <- r1Val

    /// OpCode - And
    let evalOpAnd (vm: VirtualMachine) (instruction: uint16): unit =
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

    /// OpCode - Load Register
    let evalOpLdr (vm: VirtualMachine) (instruction: uint16): unit =
        let r0 = Bits.unpack9 instruction
        let r1 = Bits.unpack6 instruction
        let r1Val = vm.ReadRegister(r1)
        let offset = Bits.signExtend (instruction &&& 0x3Fus) 6
        let memVal = vm.ReadMemoryInstruction(r1Val + offset)
        vm.WriteRegister(r0, memVal)
        vm.UpdateConditionFlags(r0)

    /// OpCode - Unused
    let evalOpRti (_: VirtualMachine) (_: uint16): unit =
        raise (badOpCodeException (OpCodeTypes.OP_RTI.ToString()))

    /// OpCode - Not
    let evalOpNot (vm: VirtualMachine) (instruction: uint16): unit =
        let r0 = Bits.unpack9 instruction
        let r1 = Bits.unpack6 instruction
        let r1Val = vm.ReadRegister(r1)
        vm.WriteRegister(r0, ~~~r1Val)
        vm.UpdateConditionFlags(r0)

    /// OpCode - Store Register
    let evalOpStr (vm: VirtualMachine) (instruction: uint16): unit =
        let r0 = Bits.unpack9 instruction
        let r1 = Bits.unpack6 instruction
        let r0Val = vm.ReadRegister(r0)
        let r1Val = vm.ReadRegister(r1)
        let offset = Bits.signExtend (instruction &&& 0x3Fus) 6
        vm.WriteMemory(r1Val + offset, r0Val)

    /// OpCode - Load Indirect
    let evalOpLdi (vm: VirtualMachine) (instruction: uint16): unit =
        let r0 = Bits.unpack9 instruction
        let offset = Bits.signExtend (instruction &&& 0x1FFus) 9
        let pcVal = vm.ReadMemoryInstruction(vm.ProgramCounter + offset)
        let memVal = vm.ReadMemoryInstruction(pcVal)
        vm.WriteRegister(r0, memVal)
        vm.UpdateConditionFlags(r0)

    /// OpCode - Store Indirect
    let evalOpSti (vm: VirtualMachine) (instruction: uint16): unit =
        let r0 = Bits.unpack9 instruction
        let r0Val = vm.ReadRegister(r0)
        let offset = Bits.signExtend (instruction &&& 0x1FFus) 9
        let memVal = vm.ReadMemoryInstruction(vm.ProgramCounter + offset)
        vm.WriteMemory(memVal, r0Val)

    /// OpCode - Jump
    let evalOpJmp (vm: VirtualMachine) (instruction: uint16): unit =
        let r1 = Bits.unpack6 instruction
        let r1Val = vm.ReadRegister(r1)
        vm.ProgramCounter <- r1Val

    /// OpCode - Unused
    let evalOpRes (_: VirtualMachine) (_: uint16): unit =
        raise (badOpCodeException (OpCodeTypes.OP_RES.ToString()))

    /// OpCode - Load Effective Address
    let evalOpLea (vm: VirtualMachine) (instruction: uint16): unit =
        let r0 = Bits.unpack9 instruction
        let offset = Bits.signExtend (instruction &&& 0x1FFus) 9
        vm.WriteRegister(r0, vm.ProgramCounter + offset)
        vm.UpdateConditionFlags(r0)

module TrapCodeEvaluator = 
    /// TrapCode - Read Single Char
    let evalOpTrapGetc (vm: VirtualMachine) (_: uint16): unit = 
        vm.R0 <- uint16 (Console.ReadKey(true).KeyChar)
        
    /// TrapCode - Write Char To Stdout
    let evalOpTrapOut (vm: VirtualMachine) (_: uint16): unit = 
        Console.Out.Write(char vm.R0)
        Console.Out.Flush()
    
    /// TrapCode - Write String To Stdout
    let evalOpTrapPuts (vm: VirtualMachine) (_: uint16): unit = 
        let mutable p = vm.R0
        let mutable c = vm.ReadMemory(p)
        while c <> 0us do
            Console.Out.Write(char c)
            p <- p + 1us
            c <- vm.ReadMemory(p)
        Console.Out.Flush()

    /// TrapCode - Get Single Char And Echo
    let evalOpTrapIn (vm: VirtualMachine) (_: uint16): unit = 
        let c = Console.ReadKey(true).KeyChar
        Console.Out.Write(c)
        vm.R0 <- uint16 c
    
    /// TrapCode - Write Multiple Chars To Stdout
    let evalOpTrapPutsp (vm: VirtualMachine) (_: uint16): unit = 
        let mutable p = vm.R0
        let mutable c = vm.ReadMemory(p)
        while c <> 0us do
            let char1 = c &&& 0xFFus;
            let char2 = c >>> 8;
            Console.Out.Write(char char1)
            Console.Out.Write(char char2)
            p <- p + 1us
            c <- vm.ReadMemory(p)
        Console.Out.Flush()
    
    /// TrapCode - Halt Program
    let evalOpTrapHalt (_: VirtualMachine) (_: uint16): unit = 
        Console.Out.Flush()
        
module VirtualEvaluator =    
    let load (vm: VirtualMachine) (reader: BinaryReader): uint16 * uint16 =
        let mutable originPtr = Bits.readUInt16 reader
        let mutable memoryPtr = originPtr
        while ((reader.BaseStream.Position <> reader.BaseStream.Length) && (memoryPtr < UInt16.MaxValue)) do
            VirtualMemory.write vm.Memory memoryPtr (Bits.readUInt16 reader)
            memoryPtr <- memoryPtr + 1us
        (originPtr, memoryPtr)

    let rec eval (vm: VirtualMachine): unit =
        let instruction = vm.ReadMemoryInstruction(vm.ProgramCounterWithIncrement)

        match (enum<OpCodeTypes> (int (Bits.unpackOp instruction))) with 
        | OpCodeTypes.OP_BR   -> evalOpBr   vm instruction 
        | OpCodeTypes.OP_ADD  -> evalOpAdd  vm instruction
        | OpCodeTypes.OP_LD   -> evalOpLd   vm instruction
        | OpCodeTypes.OP_ST   -> evalOpSt   vm instruction
        | OpCodeTypes.OP_JSR  -> evalOpJsr  vm instruction
        | OpCodeTypes.OP_AND  -> evalOpAnd  vm instruction
        | OpCodeTypes.OP_LDR  -> evalOpLdr  vm instruction
        | OpCodeTypes.OP_STR  -> evalOpStr  vm instruction
        | OpCodeTypes.OP_RTI  -> evalOpRti  vm instruction
        | OpCodeTypes.OP_NOT  -> evalOpNot  vm instruction
        | OpCodeTypes.OP_LDI  -> evalOpLdi  vm instruction
        | OpCodeTypes.OP_STI  -> evalOpSti  vm instruction
        | OpCodeTypes.OP_JMP  -> evalOpJmp  vm instruction
        | OpCodeTypes.OP_RES  -> evalOpRes  vm instruction
        | OpCodeTypes.OP_LEA  -> evalOpLea  vm instruction
        | OpCodeTypes.OP_TRAP -> evalOpTrap vm instruction
        | unknownCode ->
            raise (unknownOpCodeException (unknownCode.ToString()))
    and evalOpBr   (vm: VirtualMachine) (instruction: uint16) = OpCodeEvaluator.evalOpBr  vm instruction; eval vm
    and evalOpAdd  (vm: VirtualMachine) (instruction: uint16) = OpCodeEvaluator.evalOpAdd vm instruction; eval vm
    and evalOpLd   (vm: VirtualMachine) (instruction: uint16) = OpCodeEvaluator.evalOpLd  vm instruction; eval vm
    and evalOpSt   (vm: VirtualMachine) (instruction: uint16) = OpCodeEvaluator.evalOpSt  vm instruction; eval vm
    and evalOpJsr  (vm: VirtualMachine) (instruction: uint16) = OpCodeEvaluator.evalOpJsr vm instruction; eval vm
    and evalOpAnd  (vm: VirtualMachine) (instruction: uint16) = OpCodeEvaluator.evalOpAnd vm instruction; eval vm
    and evalOpLdr  (vm: VirtualMachine) (instruction: uint16) = OpCodeEvaluator.evalOpLdr vm instruction; eval vm
    and evalOpRti  (vm: VirtualMachine) (instruction: uint16) = OpCodeEvaluator.evalOpRti vm instruction; eval vm
    and evalOpNot  (vm: VirtualMachine) (instruction: uint16) = OpCodeEvaluator.evalOpNot vm instruction; eval vm
    and evalOpStr  (vm: VirtualMachine) (instruction: uint16) = OpCodeEvaluator.evalOpStr vm instruction; eval vm
    and evalOpLdi  (vm: VirtualMachine) (instruction: uint16) = OpCodeEvaluator.evalOpLdi vm instruction; eval vm
    and evalOpSti  (vm: VirtualMachine) (instruction: uint16) = OpCodeEvaluator.evalOpSti vm instruction; eval vm
    and evalOpJmp  (vm: VirtualMachine) (instruction: uint16) = OpCodeEvaluator.evalOpJmp vm instruction; eval vm
    and evalOpRes  (vm: VirtualMachine) (instruction: uint16) = OpCodeEvaluator.evalOpRes vm instruction; eval vm
    and evalOpLea  (vm: VirtualMachine) (instruction: uint16) = OpCodeEvaluator.evalOpLea vm instruction; eval vm
    and evalOpTrap (vm: VirtualMachine) (instruction: uint16) =
        match (enum<TrapCodeTypes> (int (Bits.unpackTrap instruction))) with 
        | TrapCodeTypes.TRAP_GETC  -> TrapCodeEvaluator.evalOpTrapGetc  vm instruction; eval vm
        | TrapCodeTypes.TRAP_OUT   -> TrapCodeEvaluator.evalOpTrapOut   vm instruction; eval vm
        | TrapCodeTypes.TRAP_PUTS  -> TrapCodeEvaluator.evalOpTrapPuts  vm instruction; eval vm
        | TrapCodeTypes.TRAP_IN    -> TrapCodeEvaluator.evalOpTrapIn    vm instruction; eval vm
        | TrapCodeTypes.TRAP_PUTSP -> TrapCodeEvaluator.evalOpTrapPutsp vm instruction; eval vm
        | TrapCodeTypes.TRAP_HALT  -> TrapCodeEvaluator.evalOpTrapHalt  vm instruction;
        | unknownCode -> 
            raise (unknownTrapCodeException (unknownCode.ToString()))