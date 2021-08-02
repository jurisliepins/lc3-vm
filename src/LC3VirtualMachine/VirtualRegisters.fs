namespace LC3VirtualMachine

open VirtualMachineTypes

module VirtualRegisters =

    let read (registers: Registers) (address: uint16): uint16 = registers.[int address]

    let write (registers: Registers) (address: uint16) (value: uint16): unit = registers.[int address] <- value

    let readProgramCounter (registers: Registers): uint16 = registers.[int RegisterTypes.R_PC]

    let writeProgramCounter (registers: Registers) (value: uint16): unit = registers.[int RegisterTypes.R_PC] <- value

    let readProgramCounterWithIncrement (registers: Registers) =
        let pc = readProgramCounter registers
        writeProgramCounter registers (pc + 1us)
        pc

    let readConditionFlag (registers: Registers): uint16 = registers.[int RegisterTypes.R_COND]

    let writeConditionFlag (registers: Registers) (value: uint16): unit = registers.[int RegisterTypes.R_COND] <- value

    let updateConditionFlags (registers: Registers) (address: uint16) =
        let rVal = read registers address
        if (rVal = 0us) then
            writeConditionFlag registers (uint16 ConditionFlagTypes.FL_ZRO)
        else if ((rVal >>> 15) <> 0us) then
            writeConditionFlag registers (uint16 ConditionFlagTypes.FL_NEG)
        else
            writeConditionFlag registers (uint16 ConditionFlagTypes.FL_POS)