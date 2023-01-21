# Little Computer 3 Virtual Machine

F# implementation of [LC3 VM](https://en.wikipedia.org/wiki/Little_Computer_3) ([Instruction Set Architecture](https://justinmeiners.github.io/lc3-vm/supplies/lc3-isa.pdf)) based on [this](https://justinmeiners.github.io/lc3-vm/) tutorial.

## Usage

### Building

From project root run `make`. Alternatively you can build the project yourself from cmd (see Makefile for examples commends) or from an IDE.

Makefile is configured to compile a self contained single binary.

### Running

From project root run `.\src\LC3VirtualMachine\bin\Release\net6.0\win-x64\lc3-vm.exe -p .\exe\2048.obj` to play 2048. A linux-x64 binary will also be compiled in the linux-64 folder.

Executable binaries are in the `exe` folder. The 2 programs are [2048](https://github.com/rpendleton/lc3-2048) and [Rogue](https://github.com/justinmeiners/lc3-rogue). 
