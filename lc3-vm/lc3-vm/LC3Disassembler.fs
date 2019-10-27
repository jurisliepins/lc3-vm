namespace lc3vm

open LC3VirtualMachine

module OpDisassembler = 
    let inline disassembleOpBr (instruction: uint16) = 
        let pcOffset = LC3Bits.signExtend2 (int (instruction &&& 0x1FFus)) 9
        let n = if ((instruction >>> 11) &&& (uint16 0b1) <> 0us) then "n" else ""
        let z = if ((instruction >>> 10) &&& (uint16 0b1) <> 0us) then "z" else ""
        let p = if ((instruction >>>  9) &&& (uint16 0b1) <> 0us) then "p" else ""
        debug (sprintf "BR%s%s%s %d\n" n z p pcOffset)
        sprintf "BR%s%s%s %d" n z p pcOffset
    
    let inline disassembleOpAdd (instruction: uint16) = 
        let dr = LC3Bits.unpack0 instruction
        match (LC3Bits.unpackImm instruction) with 
        | 0us -> 
            let sr1 = LC3Bits.unpack1 instruction
            let sr2 = LC3Bits.unpack2 instruction
            debug (sprintf "ADD R%d, R%d, R%d\n" dr sr1 sr2)
            sprintf "ADD R%d, R%d, R%d" dr sr1 sr2
        | _ -> 
            let sr1 = LC3Bits.unpack1 instruction
            let imm = LC3Bits.signExtend2 (int (instruction &&& 0x1Fus)) 5
            debug (sprintf "ADD R%d, R%d, #%d\n" dr sr1 imm)
            sprintf "ADD R%d, R%d, #%d" dr sr1 imm

    let inline disassembleOpLd (instruction: uint16) = 
        let dr = LC3Bits.unpack0 instruction
        let pcOffset = LC3Bits.signExtend2 (int (instruction &&& 0x1ffus)) 9
        debug (sprintf "LD R%d, %x\n" dr pcOffset)
        sprintf "LD R%d, %x" dr pcOffset

    let inline disassembleOpSt (instruction: uint16) = 
        let sr = LC3Bits.unpack0 instruction
        let pcOffset = LC3Bits.signExtend2 (int (instruction &&& 0x1FFus)) 9
        debug (sprintf "ST R%d, %x\n" sr pcOffset)
        sprintf "ST R%d, %x" sr pcOffset

    let inline disassembleOpJsr (instruction: uint16) = 
        match LC3Bits.unpackLong instruction with
        | 0us -> 
            let sr = LC3Bits.unpack1 instruction
            debug (sprintf "JSRR R%d\n" sr)
            sprintf "JSRR R%d" sr
        | _   -> 
            let longPcOffset = LC3Bits.signExtend (instruction &&& 0x7FFus) 11
            debug (sprintf "JSR %x\n" longPcOffset)
            sprintf "JSR %x" longPcOffset

    let inline disassembleOpAnd (instruction: uint16) = 
        let dr = LC3Bits.unpack0 instruction
        match LC3Bits.unpackImm instruction with 
        | 0us -> 
            let sr1 = LC3Bits.unpack1 instruction
            let sr2 = LC3Bits.unpack2 instruction
            debug (sprintf "AND R%d, R%d, R%d\n" dr sr1 sr2)
            sprintf "AND R%d, R%d, R%d" dr sr1 sr2
        | _ -> 
            let sr1 = LC3Bits.unpack1 instruction
            let imm = LC3Bits.signExtend2 (int (instruction &&& 0x1Fus)) 5
            debug (sprintf "AND R%d, R%d, #%d\n" dr sr1 imm)
            sprintf "AND R%d, R%d, #%d" dr sr1 imm

    let inline disassembleOpLdr (instruction: uint16) = 
        let dr = LC3Bits.unpack0 instruction
        let sr = LC3Bits.unpack1 instruction
        let pcOffset = LC3Bits.signExtend2 (int (instruction &&& 0x3Fus)) 6
        sprintf "LDR R%d, R%d, #%d\n" dr sr pcOffset

    let inline disassembleOpRti (_: uint16) = 
        ""

    let inline disassembleOpNot (instruction: uint16) = 
        let dr = LC3Bits.unpack0 instruction
        let sr = LC3Bits.unpack1 instruction
        debug (sprintf "NOT R%d, R%d\n" dr sr)
        sprintf "NOT R%d, R%d" dr sr

    let inline disassembleOpStr (instruction: uint16) = 
        let sr1 = LC3Bits.unpack0 instruction
        let sr2 = LC3Bits.unpack1 instruction
        let offset = LC3Bits.signExtend2 (int (instruction &&& 0x3Fus)) 6
        debug (sprintf "STR R%d, R%d, #%d\n" sr1 sr2 (int offset))
        sprintf "STR R%d, R%d, #%d" sr1 sr2 (int offset)

    let inline disassembleOpLdi (instruction: uint16) = 
        let dr = LC3Bits.unpack0 instruction
        let pcOffset = LC3Bits.signExtend (instruction &&& 0x1FFus) 9
        debug (sprintf "LDI R%d, %x\n" dr pcOffset)
        sprintf "LDI R%d, %x" dr pcOffset

    let inline disassembleOpSti (instruction: uint16) = 
        let sr = LC3Bits.unpack0 instruction
        let pcOffset = LC3Bits.signExtend (instruction &&& 0x1FFus) 9
        debug (sprintf "STI R%d, %x\n" sr pcOffset)
        sprintf "STI R%d, %x" sr pcOffset

    let inline disassembleOpJmp (instruction: uint16) = 
        let sr = LC3Bits.unpack1 instruction
        match sr with 
        | 7us -> 
            debug ("RET\n")
            "RET"
        | _ -> 
            debug (sprintf "JMP R%d\n" sr)
            sprintf "JMP R%d" sr

    let inline disassembleOpRes (_: uint16) = 
        ""

    let inline disassembleOpLea (instruction: uint16) = 
        let dr = LC3Bits.unpack0 instruction
        let pcOffset = LC3Bits.signExtend (instruction &&& 0x1FFus) 9
        debug (sprintf "LEA R%d, %x\n" dr pcOffset)
        sprintf "LEA R%d, %x" dr pcOffset

module TrapDisassembler = 
    //let inline disassembleOpTrapGetc (instruction: uint16) = 
    //    debug (sprintf "TRAP x%2x\n" instruction)
    //    sprintf "TRAP x%2x" instruction

    //let inline disassembleOpTrapOut (instruction: uint16) = 
    //    debug (sprintf "TRAP x%2x\n" instruction)
    //    sprintf "TRAP x%2x" instruction

    //let inline disassembleOpTrapPuts (instruction: uint16) = 
    //    debug (sprintf "TRAP x%2x\n" instruction)
    //    sprintf "TRAP x%2x" instruction

    //let inline disassembleOpTrapIn (instruction: uint16) = 
    //    debug (sprintf "TRAP x%2x\n" instruction)
    //    sprintf "TRAP x%2x" instruction

    //let inline disassembleOpTrapPutsp (instruction: uint16) = 
    //    debug (sprintf "TRAP x%2x\n" instruction)
    //    sprintf "TRAP x%2x" instruction

    let inline disassembleOpTrap (instruction: uint16) = 
        debug (sprintf "TRAP %x\n" (LC3Bits.unpackTrap instruction))
        sprintf "TRAP %x" (LC3Bits.unpackTrap instruction)

    let inline disassembleOpTrapHalt (_: uint16) = 
        debug ("HALT\n")
        "HALT"

module LC3Disassembler =
    open System
    open LC3VirtualMachineTypes
    open LC3VirtualMachineException

    let print (instructions: string[]) =
        for instruction in instructions do Console.WriteLine instruction

    let rec disassemble (vm: LC3VirtualMachine) (instructions: string[]) =
        if vm.IsProgramCounterEnd then
            ()
        else 
            let instruction = vm.ReadMemory(vm.ProgramCounterWithIncrement)

            match (enum<OpcodeTypes> (int (LC3Bits.unpackOp instruction))) with 
            | OpcodeTypes.OP_BR   -> disassembleOpBr   vm instruction instructions
            | OpcodeTypes.OP_ADD  -> disassembleOpAdd  vm instruction instructions
            | OpcodeTypes.OP_LD   -> disassembleOpLd   vm instruction instructions
            | OpcodeTypes.OP_ST   -> disassembleOpSt   vm instruction instructions
            | OpcodeTypes.OP_JSR  -> disassembleOpJsr  vm instruction instructions
            | OpcodeTypes.OP_AND  -> disassembleOpAnd  vm instruction instructions
            | OpcodeTypes.OP_LDR  -> disassembleOpLdr  vm instruction instructions
            | OpcodeTypes.OP_STR  -> disassembleOpStr  vm instruction instructions
            | OpcodeTypes.OP_RTI  -> disassembleOpRti  vm instruction instructions
            | OpcodeTypes.OP_NOT  -> disassembleOpNot  vm instruction instructions
            | OpcodeTypes.OP_LDI  -> disassembleOpLdi  vm instruction instructions
            | OpcodeTypes.OP_STI  -> disassembleOpSti  vm instruction instructions
            | OpcodeTypes.OP_JMP  -> disassembleOpJmp  vm instruction instructions
            | OpcodeTypes.OP_RES  -> disassembleOpRes  vm instruction instructions
            | OpcodeTypes.OP_LEA  -> disassembleOpLea  vm instruction instructions
            | OpcodeTypes.OP_TRAP -> disassembleOpTrap vm instruction instructions
            | unknownCode ->
                raise (unknownOpcodeException (unknownCode.ToString()))
    and disassembleOpBr (vm: LC3VirtualMachine) (instruction: uint16) (instructions: string[]) =
        instructions.[int vm.ProgramCounter - 1] <- OpDisassembler.disassembleOpBr instruction
        disassemble vm instructions
    and disassembleOpAdd (vm: LC3VirtualMachine) (instruction: uint16) (instructions: string[]) =
        instructions.[int vm.ProgramCounter - 1] <- OpDisassembler.disassembleOpAdd instruction
        disassemble vm instructions
    and disassembleOpLd (vm: LC3VirtualMachine) (instruction: uint16) (instructions: string[]) =
        instructions.[int vm.ProgramCounter - 1] <- OpDisassembler.disassembleOpLd instruction
        disassemble vm instructions
    and disassembleOpSt (vm: LC3VirtualMachine) (instruction: uint16) (instructions: string[]) =
        instructions.[int vm.ProgramCounter - 1] <- OpDisassembler.disassembleOpSt instruction
        disassemble vm instructions
    and disassembleOpJsr (vm: LC3VirtualMachine) (instruction: uint16) (instructions: string[]) =
        instructions.[int vm.ProgramCounter - 1] <- OpDisassembler.disassembleOpJsr instruction
        disassemble vm instructions
    and disassembleOpAnd (vm: LC3VirtualMachine) (instruction: uint16) (instructions: string[]) =
        instructions.[int vm.ProgramCounter - 1] <- OpDisassembler.disassembleOpAnd instruction
        disassemble vm instructions
    and disassembleOpLdr (vm: LC3VirtualMachine) (instruction: uint16) (instructions: string[]) =
        instructions.[int vm.ProgramCounter - 1] <- OpDisassembler.disassembleOpLdr instruction
        disassemble vm instructions
    and disassembleOpRti (vm: LC3VirtualMachine) (instruction: uint16) (instructions: string[]) =
        instructions.[int vm.ProgramCounter - 1] <- OpDisassembler.disassembleOpRti instruction
        disassemble vm instructions
    and disassembleOpNot (vm: LC3VirtualMachine) (instruction: uint16) (instructions: string[]) =
        instructions.[int vm.ProgramCounter - 1] <- OpDisassembler.disassembleOpNot instruction
        disassemble vm instructions
    and disassembleOpStr (vm: LC3VirtualMachine) (instruction: uint16) (instructions: string[]) =
        instructions.[int vm.ProgramCounter - 1] <- OpDisassembler.disassembleOpStr instruction
        disassemble vm instructions
    and disassembleOpLdi (vm: LC3VirtualMachine) (instruction: uint16) (instructions: string[]) =
        instructions.[int vm.ProgramCounter - 1] <- OpDisassembler.disassembleOpLdi instruction
        disassemble vm instructions
    and disassembleOpSti (vm: LC3VirtualMachine) (instruction: uint16) (instructions: string[]) =
        instructions.[int vm.ProgramCounter - 1] <- OpDisassembler.disassembleOpSti instruction
        disassemble vm instructions
    and disassembleOpJmp (vm: LC3VirtualMachine) (instruction: uint16) (instructions: string[]) =
        instructions.[int vm.ProgramCounter - 1] <- OpDisassembler.disassembleOpJmp instruction
        disassemble vm instructions
    and disassembleOpRes (vm: LC3VirtualMachine) (instruction: uint16) (instructions: string[]) =
        instructions.[int vm.ProgramCounter - 1] <- OpDisassembler.disassembleOpRes instruction
        disassemble vm instructions
    and disassembleOpLea (vm: LC3VirtualMachine) (instruction: uint16) (instructions: string[]) =
        instructions.[int vm.ProgramCounter - 1] <- OpDisassembler.disassembleOpLea instruction
        disassemble vm instructions
    and disassembleOpTrap (vm: LC3VirtualMachine) (instruction: uint16) (instructions: string[]) =
        match (enum<TrapcodeTypes> (int (LC3Bits.unpackTrap instruction))) with 
        | TrapcodeTypes.TRAP_GETC -> 
            instructions.[int vm.ProgramCounter - 1] <- TrapDisassembler.disassembleOpTrap instruction
            disassemble vm instructions
        | TrapcodeTypes.TRAP_OUT -> 
            instructions.[int vm.ProgramCounter - 1] <- TrapDisassembler.disassembleOpTrap instruction
            disassemble vm instructions
        | TrapcodeTypes.TRAP_PUTS -> 
            instructions.[int vm.ProgramCounter - 1] <- TrapDisassembler.disassembleOpTrap instruction
            disassemble vm instructions
        | TrapcodeTypes.TRAP_IN -> 
            instructions.[int vm.ProgramCounter - 1] <- TrapDisassembler.disassembleOpTrap instruction
            disassemble vm instructions
        | TrapcodeTypes.TRAP_PUTSP -> 
            instructions.[int vm.ProgramCounter - 1] <- TrapDisassembler.disassembleOpTrap instruction
            disassemble vm instructions
        | TrapcodeTypes.TRAP_HALT -> 
            instructions.[int vm.ProgramCounter - 1] <- TrapDisassembler.disassembleOpTrapHalt instruction
            disassemble vm instructions
        | unknownCode -> 
            raise (unknownTrapcodeException (unknownCode.ToString()))
        
    let disassembleVirtualMachine (vm: LC3VirtualMachine) =
        let memory, _ = vm.VirtualMachine
        let instructions = Array.create (memory.Length) ""
        disassemble vm instructions
        instructions

