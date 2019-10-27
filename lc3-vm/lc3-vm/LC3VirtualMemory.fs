namespace lc3vm

module LC3VirtualMemory =
    open System
    open lc3vm.LC3VirtualMachineTypes

    let inline read (memory: Memory) (addr: uint16) = 
        //printf "[|r:"
        if ((enum<MemoryMappedRegisterTypes> (int addr)) = MemoryMappedRegisterTypes.MR_KBSR) then
            if Console.KeyAvailable then
                memory.[int MemoryMappedRegisterTypes.MR_KBSR] <- (1us <<< 15)
                memory.[int MemoryMappedRegisterTypes.MR_KBDR] <- uint16 (Console.ReadKey(true).KeyChar)
                //printf "mr_kbsr:%d:" memory.[int MemoryMappedRegisterTypes.MR_KBSR]
                //printf "mr_kbdr:%d:" memory.[int MemoryMappedRegisterTypes.MR_KBDR]
            else
                memory.[int MemoryMappedRegisterTypes.MR_KBSR] <- 0us
                //printf "mr_kbsr:%d:" memory.[int MemoryMappedRegisterTypes.MR_KBSR]
        //printf "%d:%d|]\n" addr memory.[int addr]
        memory.[int addr]

    let inline readDirect (memory: Memory) (addr: uint16) = 
        //printf "[|r:%d:%d|]\n" addr memory.[int addr]
        memory.[int addr]
    
    let inline write (memory: Memory) (addr: uint16) (value: uint16) = 
        //printf "[|w:%d:%d|]\n" addr value
        memory.[int addr] <- value
    
    let rec dump (memory: Memory) (lower: uint16) (upper: uint16) =
        let lowerBound = int lower
        let upperBound = int upper
        match lowerBound with
        | _ when (lowerBound + 3 < upperBound) -> 
            printf "%5d "   memory.[lowerBound]
            printf "%5d "   memory.[lowerBound + 1]
            printf "%5d "   memory.[lowerBound + 2]
            printf "%5d \n" memory.[lowerBound + 3]
            dump memory (lower + 4us) upper
        | _ when (lowerBound + 2 < upperBound) -> 
            printf "%5d "   memory.[lowerBound]
            printf "%5d "   memory.[lowerBound + 1]
            printf "%5d \n" memory.[lowerBound + 2]
            dump memory (lower + 3us) upper
        | _ when (lowerBound + 1 < upperBound) -> 
            printf "%5d "   memory.[lowerBound]
            printf "%5d \n" memory.[lowerBound + 1]
            dump memory (lower + 2us) upper
        | _ when (lowerBound < upperBound) -> 
            printf "%d \n" memory.[lowerBound]
            dump memory (lower + 1us) upper
        | _ -> ()

    let rec dumpNonEmpty (memory: Memory) (ptr: uint16) =
        if (int ptr >= memory.Length) then
            ()
        else
            if (memory.[int ptr] <> 0us) then
                printfn "%d" memory.[int ptr]
            dumpNonEmpty memory (ptr + 1us)

