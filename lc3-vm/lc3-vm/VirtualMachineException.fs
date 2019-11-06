namespace lc3vm

module VirtualMachineException =
    exception VirtualMachineException of string
    
    let inline unexpectedValueException (arg: string) = 
        VirtualMachineException("Unexpected value '" + arg + "'")
    
    let inline badOpcodeException (arg: string) = 
        VirtualMachineException("Bad opcode '" + arg + "'")

    let inline unknownOpcodeException (arg: string) = 
        VirtualMachineException("Unknown opcode '" + arg + "'")

    let inline unknownTrapcodeException (arg: string) = 
        VirtualMachineException("Unknown trapcode '" + arg + "'")

    

