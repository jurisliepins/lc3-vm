namespace lc3vm

module VirtualMachineTypes =
    type RegisterTypes = 
        | R_R0    = 0
        | R_R1    = 1
        | R_R2    = 2
        | R_R3    = 3
        | R_R4    = 4
        | R_R5    = 5
        | R_R6    = 6
        | R_R7    = 7
        | R_PC    = 8
        | R_COND  = 9
        | R_COUNT = 10 

    type OpcodeTypes = 
        | OP_BR   = 0
        | OP_ADD  = 1
        | OP_LD   = 2
        | OP_ST   = 3
        | OP_JSR  = 4
        | OP_AND  = 5
        | OP_LDR  = 6
        | OP_STR  = 7
        | OP_RTI  = 8
        | OP_NOT  = 9
        | OP_LDI  = 10
        | OP_STI  = 11
        | OP_JMP  = 12
        | OP_RES  = 13
        | OP_LEA  = 14
        | OP_TRAP = 15

    type ConditionFlagTypes = 
        | FL_POS = 1
        | FL_ZRO = 2
        | FL_NEG = 4

    type MemoryMappedRegisterTypes =
        | MR_KBSR = 0xFE00
        | MR_KBDR = 0xFE02

    type TrapcodeTypes = 
        | TRAP_GETC  = 0x20
        | TRAP_OUT   = 0x21
        | TRAP_PUTS  = 0x22
        | TRAP_IN    = 0x23
        | TRAP_PUTSP = 0x24
        | TRAP_HALT  = 0x25

    type Memory = array<uint16>

    type Registers = array<uint16>

    type VirtualMachine = (Memory * Registers)