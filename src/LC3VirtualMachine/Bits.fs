namespace LC3VirtualMachine

open System.IO
open System

module Bits =
    
    let unpackOp (instruction: uint16): uint16 = (instruction >>> 12)
    
    let unpackTrap (instruction: uint16): uint16 = (instruction &&& 0xFFus)
        
    let unpack9 (instruction: uint16): uint16 = (instruction >>> 9) &&& 0x7us

    let unpack6 (instruction: uint16): uint16 = (instruction >>> 6) &&& 0x7us

    let unpack0 (instruction: uint16): uint16 = (instruction &&& 0x7us)
        
    let unpackImm (instruction: uint16): uint16 = (instruction >>> 5) &&& 0x1us
    
    let unpackLong (instruction: uint16): uint16 = (instruction >>> 11) &&& 1us

    let swapUInt16 (value: uint16): uint16 = (value <<< 8) ||| (value >>> 8)

    let readUInt16 (reader: BinaryReader): uint16 = 
        if BitConverter.IsLittleEndian then
            reader.ReadUInt16() |> swapUInt16
        else
            reader.ReadUInt16()

    let signExtend (value: uint16) (bitCount: int): uint16 = 
        if ((int value >>> (bitCount - 1)) &&& 1) <> 0 then
            uint16 (int value ||| (0xFFFF <<< bitCount))
        else
            value