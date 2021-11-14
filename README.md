# Little Computer 3 Virtual Machine

F# implmenetation of [LC3 VM](https://en.wikipedia.org/wiki/Little_Computer_3) ([Instruction Set Architecture](https://justinmeiners.github.io/lc3-vm/supplies/lc3-isa.pdf)) based on [this](https://justinmeiners.github.io/lc3-vm/) tutorial.

[![lc3-vm.gif](https://s8.gifyu.com/images/lc3-vm.gif)](https://gifyu.com/image/S2MBX)

## Building

From project root run `make`. 

## Running

From project root run `.\src\LC3VirtualMachine\bin\Release\net6.0\win-x64\lc3-vm.exe -p .\exe\2048.obj`.

## Exe

Executable binaries are in the `exe` folder. The 2 programs are [2048](https://github.com/rpendleton/lc3-2048) and [Rogue](https://github.com/justinmeiners/lc3-rogue). 
