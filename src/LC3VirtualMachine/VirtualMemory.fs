namespace LC3VirtualMachine

module VirtualMemory =
    open System
    open LC3VirtualMachine.VirtualMachineTypes

    let inline read (memory: Memory) (addr: uint16) = 
        if ((enum<MemoryMappedRegisterTypes> (int addr)) = MemoryMappedRegisterTypes.MR_KBSR) then
            if Console.KeyAvailable then
                memory.[int MemoryMappedRegisterTypes.MR_KBSR] <- (1us <<< 15)
                memory.[int MemoryMappedRegisterTypes.MR_KBDR] <- uint16 (Console.ReadKey(true).KeyChar)
            else
                memory.[int MemoryMappedRegisterTypes.MR_KBSR] <- 0us
        memory.[int addr]

    let inline readDirect (memory: Memory) (addr: uint16) = 
        memory.[int addr]
    
    let inline write (memory: Memory) (addr: uint16) (value: uint16) = 
        memory.[int addr] <- value