# Little Computer 3 Virtual Machine

F# implmenetation of [LC3 VM](https://en.wikipedia.org/wiki/Little_Computer_3) ([Instruction Set Architecture](https://justinmeiners.github.io/lc3-vm/supplies/lc3-isa.pdf)). Based on [this](https://justinmeiners.github.io/lc3-vm/) tutorial.

![](https://media.giphy.com/media/EDyebUFw2Yqq05iwcV/source.gif)

## Building

From project root call `dotnet build -c Release .\src\LC3VirtualMachine\` to build. 

Building in `Release` configuration. `Debug` will throw a StackOverflowException since tail recursion optimisations are not performed during compilation.


## Running

Call `./src/LC3VirtualMachine/bin/Release/netcoreapp3.1/LC3VirtualMachine.exe -p ./exec/2048.obj` to execute one of the binaries (Use `WSL` terminal if you have access to it).

Executable binaries are in the `exec` folder. The 2 programs are [2048](https://github.com/rpendleton/lc3-2048) and [Rogue](https://github.com/justinmeiners/lc3-rogue). 
