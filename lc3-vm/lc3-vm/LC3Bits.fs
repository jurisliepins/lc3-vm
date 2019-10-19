namespace lc3vm

module LC3Bits =
    open System.IO
    open System

    let inline unpackOp (instruction: uint16) = (instruction >>> 12)
    
    let inline unpackTrap (instruction: uint16) = (instruction &&& 0xFFus)
        
    let inline unpack0 (instruction: uint16) = (instruction >>> 9) &&& 0x7us
        
    let inline unpack1 (instruction: uint16) = (instruction >>> 6) &&& 0x7us
    
    let inline unpack2 (instruction: uint16) = (instruction &&& 0x7us)
        
    let inline unpackImm (instruction: uint16) = (instruction >>> 5) &&& 0x1us
    
    let inline unpackLong (instruction: uint16) = (instruction >>> 11) &&& 1us

    let inline swapUInt16 (value: uint16) = (value <<< 8) ||| (value >>> 8)

    let inline readUInt16 (reader: BinaryReader) = 
        if BitConverter.IsLittleEndian then reader.ReadUInt16() |> swapUInt16 else reader.ReadUInt16()

    let inline signExtend (value: uint16) (bitCount: int) = 
        if ((value >>> (bitCount - 1)) &&& 1us) > 0us then value ||| (0xFFFFus <<< bitCount) else value

