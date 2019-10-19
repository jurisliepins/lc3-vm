namespace lc3vm

module LC3VirtualRegisters =
    open lc3vm.LC3VirtualMachineTypes

    let inline read (registers: Registers) (addr: uint16) = 
        registers.[int addr]

    let inline write (registers: Registers) (addr: uint16) (value: uint16) = 
        registers.[int addr] <- value

    let inline readProgramCounter (registers: Registers) = 
        registers.[int RegisterTypes.R_PC]

    let inline writeProgramCounter (registers: Registers) (value: uint16) = 
        registers.[int RegisterTypes.R_PC] <- value

    let inline readProgramCounterWithIncerement (registers: Registers) =
        let pc = readProgramCounter registers
        writeProgramCounter registers (pc + 1us)
        pc

    let inline readConditionFlag (registers: Registers) = 
        registers.[int RegisterTypes.R_COND]

    let inline writeConditionFlag (registers: Registers) (value: uint16) = 
        registers.[int RegisterTypes.R_COND] <- value

    let inline updateConditionFlag (registers: Registers) (addr: uint16) =
        match read registers addr with
        | value when (value = 0us)          -> writeConditionFlag registers (uint16 ConditionFlagTypes.FL_ZRO)
        | value when ((value >>> 15) > 0us) -> writeConditionFlag registers (uint16 ConditionFlagTypes.FL_NEG)
        | _                                 -> writeConditionFlag registers (uint16 ConditionFlagTypes.FL_POS)

    let rec dump (registers: Registers) =
        printfn "%A" registers

