namespace lc3vm

module LC3VirtualRegisters =
    open lc3vm.LC3VirtualMachineTypes

    let inline read (registers: Registers) (addr: uint16) = 
        registers.[int addr]

    let inline write (registers: Registers) (addr: uint16) (value: uint16) = 
        registers.[int addr] <- value

    let inline readPc (registers: Registers) = 
        registers.[int RegisterTypes.R_PC]

    let inline writePc (registers: Registers) (value: uint16) = 
        registers.[int RegisterTypes.R_PC] <- value

    let inline readPcWithIncr (registers: Registers) =
        let pc = readPc registers
        writePc registers (pc + 1us)
        pc

    let inline readCondFlag (registers: Registers) = 
        registers.[int RegisterTypes.R_COND]

    let inline writeCondFlag (registers: Registers) (value: uint16) = 
        registers.[int RegisterTypes.R_COND] <- value

    let inline updateCondFlags (registers: Registers) (addr: uint16) =
        let rVal = read registers addr
        if (rVal = 0us) then
            writeCondFlag registers (uint16 ConditionFlagTypes.FL_ZRO)
        else if ((rVal >>> 15) <> 0us) then
            writeCondFlag registers (uint16 ConditionFlagTypes.FL_NEG)
        else
            writeCondFlag registers (uint16 ConditionFlagTypes.FL_POS)