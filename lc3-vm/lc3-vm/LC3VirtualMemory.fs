namespace lc3vm

module LC3VirtualMemory =
    open System
    open lc3vm.LC3VirtualMachineTypes

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
    (*
    let rec dump (memory: Memory) (lower: uint16) (upper: uint16) print =
        let lowerBound = int lower
        let upperBound = int upper
        match lowerBound with
        | _ when (lowerBound + 3 < upperBound) -> 
            print (sprintf "%5d "   memory.[lowerBound])
            print (sprintf "%5d "   memory.[lowerBound + 1])
            print (sprintf "%5d "   memory.[lowerBound + 2])
            print (sprintf "%5d \n" memory.[lowerBound + 3])
            dump memory (lower + 4us) upper print 
        | _ when (lowerBound + 2 < upperBound) -> 
            print (sprintf "%5d "   memory.[lowerBound])
            print (sprintf "%5d "   memory.[lowerBound + 1])
            print (sprintf "%5d \n" memory.[lowerBound + 2])
            dump memory (lower + 3us) upper print
        | _ when (lowerBound + 1 < upperBound) -> 
            print (sprintf "%5d "   memory.[lowerBound])
            print (sprintf "%5d \n" memory.[lowerBound + 1])
            dump memory (lower + 2us) upper print
        | _ when (lowerBound < upperBound) -> 
            print (sprintf "%d \n" memory.[lowerBound])
            dump memory (lower + 1us) upper print
        | _ -> ()

    let rec dumpNonEmpty (memory: Memory) (ptr: uint16) print =
        if (int ptr >= memory.Length) then
            ()
        else
            if (memory.[int ptr] <> 0us) then
                print (sprintf "%d\n" memory.[int ptr])
            dumpNonEmpty memory (ptr + 1us) print
    *)