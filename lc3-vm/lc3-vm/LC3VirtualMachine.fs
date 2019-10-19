namespace lc3vm

open System.IO
open System
open lc3vm.LC3VirtualMachineException
open lc3vm.LC3VirtualMachineTypes

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
        let mutable c = memory.[int p] // LC3VirtualMemory.read memory p 
        while c <> 0us do
            Console.Out.Write(char c)
            p <- p + 1us
            c <- memory.[int p] // LC3VirtualMemory.read memory p 
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
        let mutable c = memory.[int p] // LC3VirtualMemory.read memory p
        while c <> 0us do
            let char1 = c &&& 0xFFus;
            let char2 = c >>> 8;
            Console.Out.Write(char char1)
            Console.Out.Write(char char2)
            p <- p + 1us
            c <- memory.[int p]// LC3VirtualMemory.read memory p
        Console.Out.Flush()
    
    let inline evalOpTrapHalt (_: VirtualMachine) (_: uint16) = 
        //Console.WriteLine("TRAP_HALT")
        Console.Out.Flush()

module OpEvaluator = 
    let inline evalOpBr ((_, registers): VirtualMachine) (instruction: uint16) =
        let condFlag = LC3Bits.unpack0 instruction
        let pcOffset = LC3Bits.signExtend (instruction &&& 0x1FFus) 9
        let cf = LC3VirtualRegisters.readConditionFlag registers
        let pc = LC3VirtualRegisters.readProgramCounter registers
        if (condFlag &&& cf) <> 0us then
            LC3VirtualRegisters.writeProgramCounter registers (pc + pcOffset)

    let inline evalOpAdd ((_, registers): VirtualMachine) (instruction: uint16) =
        let dr = LC3Bits.unpack0 instruction
        match (LC3Bits.unpackImm instruction) with 
        //| 1us ->
        | 0us -> 
            let sr1 = LC3VirtualRegisters.read registers (LC3Bits.unpack1 instruction)
            let sr2 = LC3VirtualRegisters.read registers (LC3Bits.unpack2 instruction)
            LC3VirtualRegisters.write registers dr (sr1 + sr2)
        | unknownValue -> 
            //raise (unexpectedValueException (unknownValue.ToString()))
            let sr1 = LC3VirtualRegisters.read registers (LC3Bits.unpack1 instruction)
            let imm = LC3Bits.signExtend (instruction &&& 0x1Fus) 5
            LC3VirtualRegisters.write registers dr (sr1 + imm)
        LC3VirtualRegisters.updateConditionFlag registers dr

    let inline evalOpLd ((memory, registers): VirtualMachine) (instruction: uint16) =
        let dr = LC3Bits.unpack0 instruction
        let pc = LC3VirtualRegisters.readProgramCounter registers
        let pcOffset = LC3Bits.signExtend (instruction &&& 0x1FFus) 9
        let memValue = LC3VirtualMemory.read memory (pc + pcOffset)
        LC3VirtualRegisters.write registers dr memValue
        LC3VirtualRegisters.updateConditionFlag registers dr

    let inline evalOpSt ((memory, registers): VirtualMachine) (instruction: uint16) =
        let sr = LC3Bits.unpack0 instruction
        let pcOffset = LC3Bits.signExtend (instruction &&& 0x1FFus) 9
        LC3VirtualMemory.write memory (
            (LC3VirtualRegisters.readProgramCounter registers) + pcOffset) (LC3VirtualRegisters.read registers sr)

    let inline evalOpJsr ((_, registers): VirtualMachine) (instruction: uint16) =
        let sr = LC3Bits.unpack1 instruction
        let longPcOffset = LC3Bits.signExtend (instruction &&& 0x7FFus) 11
        let pc = LC3VirtualRegisters.readProgramCounter registers
        LC3VirtualRegisters.write registers (uint16 RegisterTypes.R_R7) pc
        match LC3Bits.unpackLong instruction with
        //| 1us -> LC3VirtualRegisters.writeProgramCounter registers (pc + longPcOffset)
        | 0us -> LC3VirtualRegisters.writeProgramCounter registers (LC3VirtualRegisters.read registers sr)
        | unknownValue -> 
            //raise (unexpectedValueException (unknownValue.ToString()))
            LC3VirtualRegisters.writeProgramCounter registers (pc + longPcOffset)

    let inline evalOpAnd ((_, registers): VirtualMachine) (instruction: uint16) =
        let dr = LC3Bits.unpack0 instruction
        match LC3Bits.unpackImm instruction with 
        //| 1us ->
        | 0us -> 
            let sr1 = LC3Bits.unpack1 instruction
            let sr2 = LC3Bits.unpack2 instruction
            LC3VirtualRegisters.write registers dr (sr1 &&& sr2)
        | unknownValue -> 
            //raise (unexpectedValueException (unknownValue.ToString()))
            let sr1 = LC3Bits.unpack1 instruction
            let imm = LC3Bits.signExtend (instruction &&& 0x1Fus) 5
            LC3VirtualRegisters.write registers dr ((LC3VirtualRegisters.read registers sr1) &&& imm)
        LC3VirtualRegisters.updateConditionFlag registers dr

    let inline evalOpLdr ((memory, registers): VirtualMachine) (instruction: uint16) =
        let dr = LC3Bits.unpack0 instruction
        let sr = LC3Bits.unpack1 instruction
        let pcOffset = LC3Bits.signExtend (instruction &&& 0x3Fus) 6
        LC3VirtualRegisters.write registers dr (LC3VirtualMemory.read memory ((LC3VirtualRegisters.read registers sr) + pcOffset))
        LC3VirtualRegisters.updateConditionFlag registers dr

    let inline evalOpRti ((_, _): VirtualMachine) (_: uint16) =
        raise (badOpcodeException (OpcodeTypes.OP_RTI.ToString()))

    let inline evalOpNot ((_, registers): VirtualMachine) (instruction: uint16) =
        let dr = LC3Bits.unpack0 instruction
        let sr = LC3Bits.unpack1 instruction
        LC3VirtualRegisters.write registers dr (LC3VirtualRegisters.read registers sr)
        LC3VirtualRegisters.updateConditionFlag registers dr

    let inline evalOpStr ((memory, registers): VirtualMachine) (instruction: uint16) =
        let sr1 = LC3Bits.unpack0 instruction
        let sr2 = LC3Bits.unpack1 instruction
        let offset = LC3Bits.signExtend (instruction &&& 0x3Fus) 6
        LC3VirtualMemory.write memory ((LC3VirtualRegisters.read registers sr2) + offset) (LC3VirtualRegisters.read registers sr1)

    let inline evalOpLdi ((memory, registers): VirtualMachine) (instruction: uint16) =
        let dr = LC3Bits.unpack0 instruction
        let pcOffset = LC3Bits.signExtend (instruction &&& 0x1FFus) 9

        //printfn "evalOpLdi"

        //LC3VirtualRegisters.dump registers

        let pc = (LC3VirtualRegisters.readProgramCounter registers)

        let a = (LC3VirtualMemory.read memory (pc + pcOffset))
        let b = (LC3VirtualMemory.read memory a)

        LC3VirtualRegisters.write registers dr b
        LC3VirtualRegisters.updateConditionFlag registers dr

        //printfn "evalOpLdi::dr       = %A" dr
        //printfn "evalOpLdi::pcOffset = %A" pcOffset

    let inline evalOpSti ((memory, registers): VirtualMachine) (instruction: uint16) =
        let dr = LC3Bits.unpack0 instruction
        let pcOffset = LC3Bits.signExtend (instruction &&& 0x1FFus) 9
        LC3VirtualMemory.write memory (
            LC3VirtualMemory.read memory ((LC3VirtualRegisters.readProgramCounter registers) + pcOffset)) (LC3VirtualRegisters.read registers dr)

    let inline evalOpJmp ((_, registers): VirtualMachine) (instruction: uint16) =
        let sr = LC3Bits.unpack1 instruction
        LC3VirtualRegisters.writeProgramCounter registers (LC3VirtualRegisters.read registers sr)

    let inline evalOpRes (_: VirtualMachine) (_: uint16) =
        raise (badOpcodeException (OpcodeTypes.OP_RES.ToString()))

    let inline evalOpLea ((_, registers): VirtualMachine) (instruction: uint16) =
        let dr = LC3Bits.unpack0 instruction
        let pc = LC3VirtualRegisters.readProgramCounter registers
        let pcOffset = LC3Bits.signExtend (instruction &&& 0x1FFus) 9
        LC3VirtualRegisters.write registers dr (pc + pcOffset)
        LC3VirtualRegisters.updateConditionFlag registers dr

    //let inline evalOpTrap (vm: VirtualMachine) (instruction: uint16) =
    //    match (enum<TrapcodeTypes> (int (LC3Bits.unpackTrap instruction))) with 
    //    | TrapcodeTypes.TRAP_GETC  -> TrapEvaluator.evalOpTrapGetc  vm instruction
    //    | TrapcodeTypes.TRAP_OUT   -> TrapEvaluator.evalOpTrapOut   vm instruction
    //    | TrapcodeTypes.TRAP_PUTS  -> TrapEvaluator.evalOpTrapPuts  vm instruction
    //    | TrapcodeTypes.TRAP_IN    -> TrapEvaluator.evalOpTrapIn    vm instruction
    //    | TrapcodeTypes.TRAP_PUTSP -> TrapEvaluator.evalOpTrapPutsp vm instruction
    //    | TrapcodeTypes.TRAP_HALT  -> TrapEvaluator.evalOpTrapHalt  vm instruction
    //    | unknownCode -> 
    //        raise (unknownTrapcodeException (unknownCode.ToString()))

module LC3VirtualMachine =
    let readProgramCounter  ((_, registers): VirtualMachine)                 = LC3VirtualRegisters.readProgramCounter  registers
    let writeProgramCounter ((_, registers): VirtualMachine) (value: uint16) = LC3VirtualRegisters.writeProgramCounter registers value

    let load ((memory, _): VirtualMachine) (path: string) =
        use reader = new BinaryReader(File.Open(path, FileMode.Open, FileAccess.Read, FileShare.Read))
            
        let mutable originPtr = LC3Bits.readUInt16 reader
        let mutable memoryPtr = originPtr

        while ((reader.BaseStream.Position <> reader.BaseStream.Length) && (memoryPtr < UInt16.MaxValue)) do
            LC3VirtualMemory.write memory memoryPtr (LC3Bits.readUInt16 reader)
            memoryPtr <- memoryPtr + 1us
        
        (originPtr, memoryPtr)

    let dumpMemory         ((memory, _):    VirtualMachine) = LC3VirtualMemory.dump memory
    let dumpNonEmptyMemory ((memory, _):    VirtualMachine) = LC3VirtualMemory.dumpNonEmpty memory
    let dumpRegisters      ((_, registers): VirtualMachine) = LC3VirtualRegisters.dump registers

    let rec eval (vm: VirtualMachine) =
        let (memory, registers) = vm

        let programCounter = LC3VirtualRegisters.readProgramCounterWithIncerement registers
        let instruction = LC3VirtualMemory.read memory programCounter

        match (enum<OpcodeTypes> (int (LC3Bits.unpackOp instruction))) with 
        | OpcodeTypes.OP_BR   -> (*Console.WriteLine("OP_BR");*)   evalOpBr   vm instruction
        | OpcodeTypes.OP_ADD  -> (*Console.WriteLine("OP_ADD");*)  evalOpAdd  vm instruction
        | OpcodeTypes.OP_LD   -> (*Console.WriteLine("OP_LD");*)   evalOpLd   vm instruction
        | OpcodeTypes.OP_ST   -> (*Console.WriteLine("OP_ST");*)   evalOpSt   vm instruction // Unchecked!
        | OpcodeTypes.OP_JSR  -> (*Console.WriteLine("OP_JSR");*)  evalOpJsr  vm instruction
        | OpcodeTypes.OP_AND  -> (*Console.WriteLine("OP_AND");*)  evalOpAnd  vm instruction
        | OpcodeTypes.OP_LDR  -> (*Console.WriteLine("OP_LDR");*)  evalOpLdr  vm instruction
        | OpcodeTypes.OP_STR  -> (*Console.WriteLine("OP_STR");*)  evalOpStr  vm instruction
        | OpcodeTypes.OP_RTI  -> (*Console.WriteLine("OP_RTI");*)  evalOpRti  vm instruction // Unchecked!
        | OpcodeTypes.OP_NOT  -> (*Console.WriteLine("OP_NOT");*)  evalOpNot  vm instruction // Unchecked!
        | OpcodeTypes.OP_LDI  -> (*Console.WriteLine("OP_LDI");*)  evalOpLdi  vm instruction
        | OpcodeTypes.OP_STI  -> (*Console.WriteLine("OP_STI");*)  evalOpSti  vm instruction // Unchecked!
        | OpcodeTypes.OP_JMP  -> (*Console.WriteLine("OP_JMP");*)  evalOpJmp  vm instruction // Unchecked!
        | OpcodeTypes.OP_RES  -> (*Console.WriteLine("OP_RES");*)  evalOpRes  vm instruction // Unchecked!
        | OpcodeTypes.OP_LEA  -> (*Console.WriteLine("OP_LEA");*)  evalOpLea  vm instruction
        | OpcodeTypes.OP_TRAP -> (*Console.WriteLine("OP_TRAP");*) evalOpTrap vm instruction
        | unknownCode ->
            printfn "%A" unknownCode
            //raise (unknownOpcodeException (unknownCode.ToString()))
    and evalOpBr (vm: VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpBr vm instruction
        //
        //dumpNonEmptyMemory vm 0us
        //dumpRegisters vm
        //
        eval vm
    and evalOpAdd (vm: VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpAdd vm instruction
        eval vm
    and evalOpLd (vm: VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpLd vm instruction
        eval vm
    and evalOpSt (vm: VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpSt vm instruction
        eval vm
    and evalOpJsr (vm: VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpJsr vm instruction
        eval vm
    and evalOpAnd (vm: VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpAnd vm instruction
        eval vm
    and evalOpLdr (vm: VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpLdr vm instruction
        eval vm
    and evalOpRti (vm: VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpRti vm instruction
        eval vm
    and evalOpNot (vm: VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpNot vm instruction
        eval vm
    and evalOpStr (vm: VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpStr vm instruction
        eval vm
    and evalOpLdi (vm: VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpLdi vm instruction
        eval vm
    and evalOpSti (vm: VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpSti vm instruction
        eval vm
    and evalOpJmp (vm: VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpJmp vm instruction
        eval vm
    and evalOpRes (vm: VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpRes vm instruction
        eval vm
    and evalOpLea (vm: VirtualMachine) (instruction: uint16) =
        OpEvaluator.evalOpLea vm instruction
        eval vm
    and evalOpTrap (vm: VirtualMachine) (instruction: uint16) =
        match (enum<TrapcodeTypes> (int (LC3Bits.unpackTrap instruction))) with 
        | TrapcodeTypes.TRAP_GETC -> 
            TrapEvaluator.evalOpTrapGetc vm instruction
            eval vm
        | TrapcodeTypes.TRAP_OUT -> 
            TrapEvaluator.evalOpTrapOut vm instruction
            eval vm
        | TrapcodeTypes.TRAP_PUTS -> 
            TrapEvaluator.evalOpTrapPuts vm instruction
            eval vm
        | TrapcodeTypes.TRAP_IN -> 
            TrapEvaluator.evalOpTrapIn vm instruction
            eval vm
        | TrapcodeTypes.TRAP_PUTSP -> 
            TrapEvaluator.evalOpTrapPutsp vm instruction
            eval vm
        | TrapcodeTypes.TRAP_HALT -> 
            TrapEvaluator.evalOpTrapHalt  vm instruction
        | unknownCode -> 
            raise (unknownTrapcodeException (unknownCode.ToString()))