namespace LC3VirtualMachine

open System
open VirtualMachineTypes

module VirtualMemory =
    let readInstruction (memory: Memory) (address: uint16): uint16 = 
        if ((enum<MemoryMappedRegisterTypes> (int address)) = MemoryMappedRegisterTypes.MR_KBSR) then
            if Console.KeyAvailable then
                memory.[int MemoryMappedRegisterTypes.MR_KBSR] <- (1us <<< 15)
                memory.[int MemoryMappedRegisterTypes.MR_KBDR] <- uint16 (Console.ReadKey(true).KeyChar)
            else
                memory.[int MemoryMappedRegisterTypes.MR_KBSR] <- 0us
        memory.[int address]
    
    let read (memory: Memory) (address: uint16): uint16 = memory.[int address]
    
    let write (memory: Memory) (address: uint16) (value: uint16): unit = memory.[int address] <- value