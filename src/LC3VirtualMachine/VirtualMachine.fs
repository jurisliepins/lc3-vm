namespace LC3VirtualMachine

open VirtualMachineTypes

type VirtualMachine(pc: uint16) =
    let memory    = createMemory ()
    let registers = createRegisters ()

    do
        VirtualRegisters.writeProgramCounter registers pc
    
    member public this.Memory with get() = memory
    
    member public this.Registers with get() = registers
    
    member public this.ReadRegister (address: uint16): uint16 =
        VirtualRegisters.read registers address

    member public this.WriteRegister(address: uint16, value: uint16): unit =
        VirtualRegisters.write registers address value

    member public this.ReadMemoryInstruction(address: uint16): uint16 =
        VirtualMemory.readInstruction memory address
    
    member public this.ReadMemory(address: uint16): uint16 =
        VirtualMemory.read memory address

    member public this.WriteMemory(address: uint16, value: uint16): unit =
        VirtualMemory.write memory address value

    member public this.UpdateConditionFlags(address: uint16): unit =
        VirtualRegisters.updateConditionFlags registers address

    member public this.R0
        with get() =
            this.ReadRegister(uint16 RegisterTypes.R_R0)
        and set(value: uint16) =
            this.WriteRegister(uint16 RegisterTypes.R_R0, value)

    member public this.R1
        with get() =
            this.ReadRegister(uint16 RegisterTypes.R_R1)
        and set(value: uint16) =
            this.WriteRegister(uint16 RegisterTypes.R_R1, value)

    member public this.R2
        with get() =
            this.ReadRegister(uint16 RegisterTypes.R_R2)
        and set(value: uint16) =
            this.WriteRegister(uint16 RegisterTypes.R_R2, value)

    member public this.R3
        with get() =
            this.ReadRegister(uint16 RegisterTypes.R_R3)
        and set(value: uint16) =
            this.WriteRegister(uint16 RegisterTypes.R_R3, value)

    member public this.R4
        with get() =
            this.ReadRegister(uint16 RegisterTypes.R_R4)
        and set(value: uint16) =
            this.WriteRegister(uint16 RegisterTypes.R_R4, value)

    member public this.R5
        with get() =
            this.ReadRegister(uint16 RegisterTypes.R_R5)
        and set(value: uint16) =
            this.WriteRegister(uint16 RegisterTypes.R_R5, value)

    member public this.R6
        with get() =
            this.ReadRegister(uint16 RegisterTypes.R_R6)
        and set(value: uint16) =
            this.WriteRegister(uint16 RegisterTypes.R_R6, value)

    member public this.R7
        with get() =
            this.ReadRegister(uint16 RegisterTypes.R_R7)
        and set(value: uint16) =
            this.WriteRegister(uint16 RegisterTypes.R_R7, value)

    member public this.ProgramCounter 
        with get() =
            VirtualRegisters.readProgramCounter registers
        and set(value: uint16) =
            VirtualRegisters.writeProgramCounter registers value

    member public this.ProgramCounterWithIncrement 
        with get() = VirtualRegisters.readProgramCounterWithIncrement registers

    member public this.ConditionFlag 
        with get() =
            VirtualRegisters.readConditionFlag registers
        and set(value: uint16) =
            VirtualRegisters.writeConditionFlag registers value