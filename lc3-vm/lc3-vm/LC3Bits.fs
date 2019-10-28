namespace lc3vm

module LC3Bits =
    open System.IO
    open System

    let inline unpackOp (instruction: uint16) = uint16 (int instruction >>> 12)
    let inline unpackTrap (instruction: uint16) = uint16 (int instruction &&& 0xFF)
        
    let inline unpack9 (instruction: uint16) = uint16 ((int instruction >>> 9) &&& 0x7)
    let inline unpack6 (instruction: uint16) = uint16 ((int instruction >>> 6) &&& 0x7)
    let inline unpack0 (instruction: uint16) = uint16 ((int instruction &&& 0x7))
        
    let inline unpackImm (instruction: uint16) = uint16 ((int instruction >>> 5) &&& 0x1)
    let inline unpackLong (instruction: uint16) = uint16 ((int instruction >>> 11) &&& 1)

    let inline swapUInt16 (value: uint16) = (value <<< 8) ||| (value >>> 8)

    let inline readUInt16 (reader: BinaryReader) = 
        if BitConverter.IsLittleEndian then reader.ReadUInt16() |> swapUInt16 else reader.ReadUInt16()

    let inline signExtend (value: uint16) (bitCount: int) = 
        if ((int value >>> (bitCount - 1)) &&& 1) <> 0 then uint16 (int value ||| (0xFFFF <<< bitCount)) else value