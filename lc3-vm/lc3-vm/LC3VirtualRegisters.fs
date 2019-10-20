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

    let inline updateConditionFlags (registers: Registers) (addr: uint16) =
        let rValue = read registers addr
        if (rValue = 0us) then
            writeConditionFlag registers (uint16 ConditionFlagTypes.FL_ZRO)
        else if ((rValue >>> 15) = 1us) then
            writeConditionFlag registers (uint16 ConditionFlagTypes.FL_NEG)
        else
            writeConditionFlag registers (uint16 ConditionFlagTypes.FL_POS)
        //match (read registers addr) with
        //| 0us                                -> writeConditionFlag registers (uint16 ConditionFlagTypes.FL_ZRO)
        //| value when ((value >>> 15) <> 0us) -> writeConditionFlag registers (uint16 ConditionFlagTypes.FL_NEG)
        //| _                                  -> writeConditionFlag registers (uint16 ConditionFlagTypes.FL_POS)

    let rec dump (registers: Registers) =
        printfn "%A" registers

