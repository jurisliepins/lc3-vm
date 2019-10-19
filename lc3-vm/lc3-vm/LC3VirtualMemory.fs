namespace lc3vm

module LC3VirtualMemory =
    open System
    open System.Net.Sockets
    open System.Linq
    open lc3vm.LC3VirtualMachineTypes

    let inline private checkKey () =
        //let mutable readfds = ([ Console.In; ].ToList())
        //Socket.Select(readfds, null, null, 0);
        //readfds.Count <> 0
        true

    let inline read (memory: Memory) (addr: uint16) = 
        //printfn "read = %d" addr

        if ((enum<MemoryMappedRegisterTypes> (int addr)) = MemoryMappedRegisterTypes.MR_KBSR) then
            if checkKey () then
                memory.[int MemoryMappedRegisterTypes.MR_KBSR] <- (1us <<< 15)
                memory.[int MemoryMappedRegisterTypes.MR_KBDR] <- uint16 (Console.ReadKey(true).KeyChar)
                //printfn "MR_KBSR(%d) = %d" (int MemoryMappedRegisterTypes.MR_KBSR) memory.[int MemoryMappedRegisterTypes.MR_KBSR]
                //printfn "MR_KBDR(%d) = %d" (int MemoryMappedRegisterTypes.MR_KBDR) memory.[int MemoryMappedRegisterTypes.MR_KBDR]
            else
                memory.[int MemoryMappedRegisterTypes.MR_KBSR] <- 0us
        memory.[int addr]
    
    let inline write (memory: Memory) (addr: uint16) (value: uint16) = 
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

