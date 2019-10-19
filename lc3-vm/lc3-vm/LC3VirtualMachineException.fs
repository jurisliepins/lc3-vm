namespace lc3vm

module LC3VirtualMachineException =
    exception LC3VirtualMachineException of string
    
    let inline unexpectedValueException (arg: string) = 
        LC3VirtualMachineException("Unexpected value '" + arg + "'")
    
    let inline badOpcodeException (arg: string) = 
        LC3VirtualMachineException("Bad opcode '" + arg + "'")

    let inline unknownOpcodeException (arg: string) = 
        LC3VirtualMachineException("Unknown opcode '" + arg + "'")

    let inline unknownTrapcodeException (arg: string) = 
        LC3VirtualMachineException("Unknown trapcode '" + arg + "'")

    

