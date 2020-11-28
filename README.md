# Little Computer 3 Virtual Machine

F# implmenetation of [LC3 VM](https://en.wikipedia.org/wiki/Little_Computer_3) ([Instruction Set Architecture](https://justinmeiners.github.io/lc3-vm/supplies/lc3-isa.pdf)), based on [this](https://justinmeiners.github.io/lc3-vm/) tutorial.

![](https://media.giphy.com/media/4Y8UNE1QIQWqaqinQn/source.gif)

## Building

From project root call 

```
dotnet build -c Release .\src\
```

to build in `Release` configuration (`Debug` will throw a StackOverflowException since tail recursion optimisations are not performed during compilation).


## Running

Call 


```
./src/bin/Release/netcoreapp3.1/lc3-vm.exe -p ./binaries/2048.obj
```

to execute one of the binaries (Use `WSL` terminal if you have access to it).

Binaries that can be ran are in the `binaries` folder. The 2 programs are - [2048](https://github.com/rpendleton/lc3-2048) and [Rogue](https://github.com/justinmeiners/lc3-rogue). 
